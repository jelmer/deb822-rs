use apt_sources::{Repositories, traits::Repository as RepositoryTrait};
use indoc::indoc;

pub const TEXT: &str = indoc! {r#"
    Types: deb
    URIs: https://download.docker.com/linux/ubuntu
    Suites: noble
    Components: stable
    Architectures: amd64
    Signed-By: /usr/share/keyrings/docker.gpg
"#};

pub fn main() {
    let repos = TEXT.parse::<Repositories>().unwrap();
    let suites = repos[0].suites();
    println!("{}", suites.join(" "));
}
