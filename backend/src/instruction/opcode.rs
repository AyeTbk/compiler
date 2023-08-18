use macros::stringify_lowercase;

macro_rules! define_opcodes {
    ($($variant:ident),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    InvalidInstruction, // Useless. Used for recoverable parsing.
    Store, // Store to a memory location.
    Load, // Load from a memory location.
    Address, // Get the address of a stack or data variable. Using it on a virtual will force it to be spilled.
    Move, // Move an operand value to a variable.
    Convert, // Convert to another type. ex: u64 to u32, i64 to u64, f64 to u64, etc.
    Call, // Call a procedure.
    Ret, // Return from a procedure.
    Jump, // Jump to a block of the same procedure. Can have a condition.
    Add, // Addition.
    Sub, // Subtraction.
}
