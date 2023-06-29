use proc_macro::TokenStream;

#[proc_macro]
pub fn stringify_lowercase(ts: TokenStream) -> TokenStream {
    let lower = ts.to_string().to_ascii_lowercase();
    format!(r#""{lower}""#).parse().unwrap()
}
