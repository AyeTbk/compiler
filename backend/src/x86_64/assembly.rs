use crate::module::Module;

pub fn generate_assembly(_module: &Module) -> String {
    r#"
main:
    mov $0, %eax
    jmp _exit
"#
    .to_string()
}
