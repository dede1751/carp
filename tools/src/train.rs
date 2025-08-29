use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use clap::Args;
use serde::Deserialize;

use bullet::{
    game::inputs::Chess768,
    nn::optimiser::AdamW,
    trainer::{
        save::SavedFormat,
        schedule::{lr, wdl, TrainingSchedule, TrainingSteps},
        settings::LocalSettings,
    },
    value::{loader::DirectSequentialDataLoader, ValueTrainerBuilder},
};

#[derive(Debug, Deserialize)]
struct Config {
    pub name: String,
    pub hidden_size: usize,

    pub num_superbatch: usize,
    pub batch_size: usize,
    pub superbatch_size: usize,

    pub initial_lr: f32,
    pub final_lr: f32,
    pub wdl_fraction: f32,
    pub eval_scale: f32,

    pub loader_threads: usize,
    pub batch_queue_size: usize,

    pub save_freq: usize,
    pub save_dir: PathBuf,
    pub data_dir: PathBuf,
}

impl Config {
    fn from_file(path: &Path) -> Result<Self> {
        let txt = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read {}", path.display()))?;
        let cfg: Self = toml::from_str(&txt).with_context(|| "Invalid TOML config")?;
        Ok(cfg)
    }

    fn batches_per_superbatch(&self) -> usize {
        (self.superbatch_size as f32 / self.batch_size as f32).ceil() as usize
    }
}

/// Train Carp's NNUE network using Bullet.
#[derive(Args)]
pub struct TrainerOptions {
    /// Path to trainer TOML config file.
    config_file: PathBuf,
}

pub fn run_trainer(opts: TrainerOptions) {
    let cfg = Config::from_file(&opts.config_file).unwrap_or_else(|e| {
        panic!(
            "Failed to load config {}: {e:#}",
            opts.config_file.display()
        )
    });

    let mut trainer = ValueTrainerBuilder::default()
        .dual_perspective()
        .optimiser(AdamW)
        .inputs(Chess768)
        .save_format(&[
            SavedFormat::id("l0w").quantise::<i16>(255),
            SavedFormat::id("l0b").quantise::<i16>(255),
            SavedFormat::id("l1w").quantise::<i16>(64),
            SavedFormat::id("l1b").quantise::<i16>(255 * 64),
        ])
        .loss_fn(|output, target| output.sigmoid().squared_error(target))
        .build(|builder, stm_inputs, ntm_inputs| {
            // weights
            let l0 = builder.new_affine("l0", 768, cfg.hidden_size);
            let l1 = builder.new_affine("l1", 2 * cfg.hidden_size, 1);

            // inference
            let stm_hidden = l0.forward(stm_inputs).screlu();
            let ntm_hidden = l0.forward(ntm_inputs).screlu();
            let hidden_layer = stm_hidden.concat(ntm_hidden);
            l1.forward(hidden_layer)
        });

    let schedule = TrainingSchedule {
        net_id: cfg.name.clone(),
        eval_scale: cfg.eval_scale,
        steps: TrainingSteps {
            batch_size: cfg.batch_size,
            batches_per_superbatch: cfg.batches_per_superbatch(),
            start_superbatch: 1,
            end_superbatch: cfg.num_superbatch,
        },
        wdl_scheduler: wdl::ConstantWDL {
            value: cfg.wdl_fraction,
        },
        lr_scheduler: lr::CosineDecayLR {
            initial_lr: cfg.initial_lr,
            final_lr: cfg.final_lr,
            final_superbatch: cfg.num_superbatch,
        },
        save_rate: cfg.save_freq,
    };

    let settings = LocalSettings {
        threads: cfg.loader_threads,
        test_set: None,
        output_directory: cfg.save_dir.to_str().unwrap(),
        batch_queue_size: cfg.batch_queue_size,
    };

    let dataloader = DirectSequentialDataLoader::new(&[cfg.data_dir.to_str().unwrap()]);

    trainer.run(&schedule, &settings, &dataloader);
}
