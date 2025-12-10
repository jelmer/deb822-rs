//! A simple tool to query PPA information from Launchpad.

use apt_sources::launchpad::{self, Error, PpaInfo, PpaSigningKey, PpaValidationResult};
use clap::{Parser, Subcommand};
use std::process::ExitCode;

#[derive(Parser)]
#[command(name = "ppa-info")]
#[command(about = "Query PPA information from Launchpad")]
#[command(version)]
struct Cli {
    /// Enable verbose output
    #[arg(short, long)]
    verbose: bool,

    /// Require authentication (for private PPAs)
    #[arg(short, long)]
    login: bool,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Show information about a PPA
    Info {
        /// PPA specification (e.g., "ppa:user/name" or "user/name")
        ppa: String,
    },
    /// Get the signing key for a PPA
    Key {
        /// PPA specification (e.g., "ppa:user/name" or "user/name")
        ppa: String,

        /// Output only the key fingerprint
        #[arg(long)]
        fingerprint_only: bool,
    },
    /// Get the subscription URL for a private PPA
    SubscriptionUrl {
        /// PPA specification (e.g., "ppa:user/name" or "user/name")
        ppa: String,
    },
}

fn parse_ppa(ppa: &str) -> Result<PpaInfo, String> {
    PpaInfo::parse(ppa)
}

fn show_info(ppa_info: &PpaInfo, auth_required: bool) -> Result<(), Error> {
    let result: PpaValidationResult = launchpad::validate_ppa(ppa_info, auth_required)?;

    if !result.exists {
        println!("PPA {}/{} does not exist", ppa_info.user, ppa_info.name);
        return Ok(());
    }

    println!("PPA: {}/{}", ppa_info.user, ppa_info.name);
    println!("Display Name: {}", result.display_name);
    println!("Private: {}", if result.is_private { "yes" } else { "no" });
    println!(
        "Debug Symbols: {}",
        if result.publishes_debug_symbols {
            "yes"
        } else {
            "no"
        }
    );

    Ok(())
}

fn show_key(ppa_info: &PpaInfo, auth_required: bool, fingerprint_only: &bool) -> Result<(), Error> {
    let signing_key: PpaSigningKey = launchpad::download_ppa_signing_key(ppa_info, auth_required)?;

    if *fingerprint_only {
        println!("{}", signing_key.fingerprint);
    } else {
        println!("Fingerprint: {}", signing_key.fingerprint);
        println!();
        println!("{}", signing_key.key_data);
    }

    Ok(())
}

fn show_subscription_url(ppa_info: &PpaInfo) -> Result<(), Error> {
    let url = launchpad::get_private_ppa_url(ppa_info)?;
    println!("{}", url);
    Ok(())
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    if cli.verbose {
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug")).init();
    } else {
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("warn")).init();
    }

    let result = match &cli.command {
        Commands::Info { ppa } => {
            let ppa_info = match parse_ppa(&ppa) {
                Ok(info) => info,
                Err(e) => {
                    eprintln!("Error: {}", e);
                    return ExitCode::FAILURE;
                }
            };
            show_info(&ppa_info, cli.login)
        }
        Commands::Key {
            ppa,
            fingerprint_only,
        } => {
            let ppa_info = match parse_ppa(&ppa) {
                Ok(info) => info,
                Err(e) => {
                    eprintln!("Error: {}", e);
                    return ExitCode::FAILURE;
                }
            };
            show_key(&ppa_info, cli.login, fingerprint_only)
        }
        Commands::SubscriptionUrl { ppa } => {
            let ppa_info = match parse_ppa(&ppa) {
                Ok(info) => info,
                Err(e) => {
                    eprintln!("Error: {}", e);
                    return ExitCode::FAILURE;
                }
            };
            show_subscription_url(&ppa_info)
        }
    };

    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("Error: {}", e);
            ExitCode::FAILURE
        }
    }
}
