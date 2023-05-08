macro_rules! define_opcodes {
    ($($variant:ident),* $(,)?) => {
        #[derive(Debug)]
        pub enum Opcode {
            $($variant),*
        }

        impl Opcode {
            pub const fn variants() -> &'static [Self] {
                &[
                    $(Self::$variant),*
                ]
            }

            pub const fn as_str(&self) -> &'static str {
                // TODO find a satisfying way to make this lowercase instead of PascalCase
                match self {
                    $(Self::$variant => stringify!($variant)),*
                }
            }

            pub fn from_str(s: &str) -> Option<Self> {
                $(
                    if s.eq_ignore_ascii_case(Self::$variant.as_str()) {
                        return Some(Self::$variant);
                    }
                )*

                None
            }
        }


    };
}

define_opcodes! {
    Store,
    Load,
    Jump,
    Call,
    Add,
    Sub,
    Ret,
}