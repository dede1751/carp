#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
#[repr(transparent)]
pub struct U64Le(u64);

impl U64Le {
    pub const fn new(v: u64) -> Self {
        Self(v.to_le())
    }

    pub const fn get(self) -> u64 {
        u64::from_le(self.0)
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
#[repr(transparent)]
pub struct U16Le(u16);

impl U16Le {
    pub const fn new(v: u16) -> Self {
        Self(v.to_le())
    }

    pub const fn get(self) -> u16 {
        u16::from_le(self.0)
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
#[repr(transparent)]
pub struct I16Le(i16);

impl I16Le {
    pub const fn new(v: i16) -> Self {
        Self(v.to_le())
    }

    pub const fn get(self) -> i16 {
        i16::from_le(self.0)
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
#[repr(transparent)]
pub struct U4Array32([u8; 16]);

impl U4Array32 {
    pub const fn get(&self, i: usize) -> u8 {
        (self.0[i / 2] >> ((i % 2) * 4)) & 0xF
    }

    pub fn set(&mut self, i: usize, v: u8) {
        debug_assert!(v < 0x10);
        self.0[i / 2] |= v << ((i % 2) * 4);
    }
}
