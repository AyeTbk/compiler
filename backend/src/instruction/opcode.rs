use macros::stringify_lowercase;

macro_rules! define_opcodes {
    ($($variant:ident),* $(,)?) => {
        #[derive(Debug, Clone, Copy)]
        pub enum Opcode {
            $($variant),*
        }

        impl Opcode {
            pub const fn variants() -> &'static [Self] {
                &[
                    $(Self::$variant),*
                ]
            }

            pub const fn to_str(&self) -> &'static str {
                match self {
                    $(Self::$variant => stringify_lowercase!($variant)),*
                }
            }

            pub fn from_str(s: &str) -> Option<Self> {
                $(
                    if s.eq_ignore_ascii_case(Self::$variant.to_str()) {
                        return Some(Self::$variant);
                    }
                )*

                None
            }
        }
    };
}

define_opcodes! {
    InvalidInstruction,
    Store,
    Load,
    Call,
    Ret,
    Jump,
    Add,
    Sub,
}
