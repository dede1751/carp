#[allow(
    dead_code,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    clippy::all,
    clippy::pedantic
)]
#[cfg(feature = "syzygy")]
mod bindings;
pub mod probe;
