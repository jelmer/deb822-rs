//! Keyserver operations for downloading and verifying GPG keys
//!
//! This module provides functionality for interacting with keyservers using Sequoia.
//! It requires the `keyserver` feature to be enabled.

#![cfg(feature = "keyserver")]

use sequoia_net::KeyServer;
use sequoia_openpgp::{parse::Parse, serialize::Marshal, Cert, KeyHandle};

/// Download a GPG key from a keyserver
///
/// # Arguments
/// * `fingerprint` - The fingerprint of the key to download (hex format)
/// * `keyserver_url` - The URL of the keyserver (e.g., "hkps://keys.openpgp.org")
///
/// # Returns
/// The key in ASCII-armored format
pub async fn download_key_from_keyserver(
    fingerprint: &str,
    keyserver_url: &str,
) -> Result<String, String> {
    // Create a KeyHandle from the fingerprint
    let key_handle: KeyHandle = fingerprint
        .parse()
        .map_err(|e| format!("Invalid fingerprint format: {}", e))?;

    // Create a keyserver client
    let keyserver = KeyServer::new(keyserver_url)
        .map_err(|e| format!("Failed to create keyserver client: {}", e))?;

    // Download the certificates (keyserver may return multiple)
    let certs = keyserver
        .get(key_handle)
        .await
        .map_err(|e| format!("Failed to download key from keyserver: {}", e))?;

    // Get the first certificate
    let cert = certs
        .into_iter()
        .next()
        .ok_or_else(|| "No certificates found for the given fingerprint".to_string())?
        .map_err(|e| format!("Failed to parse certificate: {}", e))?;

    // Serialize the certificate to armored format
    let mut armored = Vec::new();
    cert.armored()
        .serialize(&mut armored)
        .map_err(|e| format!("Failed to serialize key: {}", e))?;

    String::from_utf8(armored).map_err(|e| format!("Failed to convert key to string: {}", e))
}

/// Download a GPG key from a keyserver (synchronous wrapper)
///
/// This is a convenience function for synchronous code that internally
/// creates a Tokio runtime to execute the async operation.
///
/// # Arguments
/// * `fingerprint` - The fingerprint of the key to download (hex format)
/// * `keyserver_url` - The URL of the keyserver (e.g., "hkps://keys.openpgp.org")
///
/// # Returns
/// The key in ASCII-armored format
pub fn download_key_from_keyserver_sync(
    fingerprint: &str,
    keyserver_url: &str,
) -> Result<String, String> {
    // Check if we're already in a tokio runtime
    if let Ok(handle) = tokio::runtime::Handle::try_current() {
        // We're in a runtime, use block_in_place
        tokio::task::block_in_place(|| {
            handle.block_on(download_key_from_keyserver(fingerprint, keyserver_url))
        })
    } else {
        // No runtime, create one
        let runtime = tokio::runtime::Runtime::new()
            .map_err(|e| format!("Failed to create async runtime: {}", e))?;
        runtime.block_on(download_key_from_keyserver(fingerprint, keyserver_url))
    }
}

/// Verify that a key has the expected fingerprint
///
/// # Arguments
/// * `key_data` - The key data in ASCII-armored or binary format
/// * `expected_fingerprint` - The expected fingerprint (spaces and case are ignored)
///
/// # Returns
/// Ok(()) if the fingerprint matches, Err with details otherwise
pub fn verify_key_fingerprint(key_data: &str, expected_fingerprint: &str) -> Result<(), String> {
    // Parse the certificate from the armored data
    let cert =
        Cert::from_bytes(key_data.as_bytes()).map_err(|e| format!("Failed to parse key: {}", e))?;

    // Get the fingerprint of the primary key
    let actual_fingerprint = cert.fingerprint().to_hex();

    // Normalize both fingerprints for comparison (remove spaces, convert to uppercase)
    let normalize = |fp: &str| -> String {
        fp.chars()
            .filter(|c| c.is_alphanumeric())
            .collect::<String>()
            .to_uppercase()
    };

    let expected_normalized = normalize(expected_fingerprint);
    let actual_normalized = normalize(&actual_fingerprint);

    // Handle both short (16 char) and long (40 char) fingerprints
    if expected_normalized.len() == 16 {
        // Short fingerprint - compare with the last 16 chars of actual
        if actual_normalized.len() >= 16
            && expected_normalized != &actual_normalized[actual_normalized.len() - 16..]
        {
            return Err(format!(
                "Fingerprint mismatch: expected {}, got {}",
                expected_fingerprint, actual_fingerprint
            ));
        }
    } else if expected_normalized != actual_normalized {
        return Err(format!(
            "Fingerprint mismatch: expected {}, got {}",
            expected_fingerprint, actual_fingerprint
        ));
    }

    Ok(())
}

/// Parse a key and extract its metadata
///
/// # Arguments
/// * `key_data` - The key data in ASCII-armored or binary format
///
/// # Returns
/// A tuple of (fingerprint, user_ids) where user_ids is a vector of email addresses
pub fn parse_key_metadata(key_data: &str) -> Result<(String, Vec<String>), String> {
    // Parse the certificate
    let cert =
        Cert::from_bytes(key_data.as_bytes()).map_err(|e| format!("Failed to parse key: {}", e))?;

    // Get the fingerprint
    let fingerprint = cert.fingerprint().to_hex();

    // Extract user IDs (email addresses)
    let user_ids: Vec<String> = cert
        .userids()
        .filter_map(|uid| uid.email2().ok().flatten().map(String::from))
        .collect();

    Ok((fingerprint, user_ids))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_fingerprint() {
        let normalize = |fp: &str| -> String {
            fp.chars()
                .filter(|c| c.is_alphanumeric())
                .collect::<String>()
                .to_uppercase()
        };

        assert_eq!(normalize("1234 5678 90AB CDEF"), "1234567890ABCDEF");
        assert_eq!(normalize("1234567890abcdef"), "1234567890ABCDEF");
        assert_eq!(normalize("12:34:56:78:90:AB:CD:EF"), "1234567890ABCDEF");
        assert_eq!(normalize("0x1234567890ABCDEF"), "0X1234567890ABCDEF");
    }

    #[test]
    fn test_verify_fingerprint_matching() {
        // Use a real test key - this is a minimal valid OpenPGP key
        let key_data = r#"-----BEGIN PGP PUBLIC KEY BLOCK-----

mDMEZIYC9xYJKwYBBAHaRw8BAQdAz5feTnR7DwGfLHLkBhoHu6GTFprNle/n/Iup
fTUT6Z60BlRlc3QgMYiZBBMWCgBBFiEE7t0zdTa4BwHfZTj0iL0eQBFT2xYFAmSG
AvcCGwMFCQPCZwAFCwkIBwICIgIGFQoJCAsCBBYCAwECHgcCF4AACgkQiL0eQBFT
2xYkEgD/b3p0QehuzJiuJLijVKOB7WKnLbnt2g8cbW7EARDHkWYBANREqydl1OYJ
c7B8N9l1cG2TCem0K3SXD8p1ELDs2aEJuDgEZIYC9xIKKwYBBAGXVQEFAQEHQOrf
4RAemEw5X5MBceW1BpYtKp+jH5ypaxpILGz7OVIfAwEIB4h+BBgWCgAmFiEE7t0z
dTa4BwHfZTj0iL0eQBFT2xYFAmSGAvcCGwwFCQPCZwAACgkQiL0eQBFT2xbL8gEA
2NenoDwxr8aWnlhajSJZz8UYNkzJNJQCPG2cukPNf3YA/RYhzCxJMkMYJ3DXtiUh
UqZBMYWFftpFkh5E5FGqs7kO
=Rt6r
-----END PGP PUBLIC KEY BLOCK-----"#;

        // The actual fingerprint of this test key (as parsed by Sequoia)
        let fingerprint = "7607A01349161EA59DC551A654F610003149BA6E";

        // Test exact match
        assert!(verify_key_fingerprint(key_data, fingerprint).is_ok());

        // Test with spaces
        assert!(verify_key_fingerprint(
            key_data,
            "7607 A013 4916 1EA5 9DC5 51A6 54F6 1000 3149 BA6E"
        )
        .is_ok());

        // Test lowercase
        assert!(
            verify_key_fingerprint(key_data, "7607a01349161ea59dc551a654f610003149ba6e").is_ok()
        );

        // Test short fingerprint (last 16 chars)
        assert!(verify_key_fingerprint(key_data, "54F610003149BA6E").is_ok());

        // Test wrong fingerprint
        assert!(
            verify_key_fingerprint(key_data, "0000000000000000000000000000000000000000").is_err()
        );
    }

    #[test]
    fn test_parse_key_metadata() {
        // Use the same valid test key
        let key_data = r#"-----BEGIN PGP PUBLIC KEY BLOCK-----

mDMEZIYC9xYJKwYBBAHaRw8BAQdAz5feTnR7DwGfLHLkBhoHu6GTFprNle/n/Iup
fTUT6Z60BlRlc3QgMYiZBBMWCgBBFiEE7t0zdTa4BwHfZTj0iL0eQBFT2xYFAmSG
AvcCGwMFCQPCZwAFCwkIBwICIgIGFQoJCAsCBBYCAwECHgcCF4AACgkQiL0eQBFT
2xYkEgD/b3p0QehuzJiuJLijVKOB7WKnLbnt2g8cbW7EARDHkWYBANREqydl1OYJ
c7B8N9l1cG2TCem0K3SXD8p1ELDs2aEJuDgEZIYC9xIKKwYBBAGXVQEFAQEHQOrf
4RAemEw5X5MBceW1BpYtKp+jH5ypaxpILGz7OVIfAwEIB4h+BBgWCgAmFiEE7t0z
dTa4BwHfZTj0iL0eQBFT2xYFAmSGAvcCGwwFCQPCZwAACgkQiL0eQBFT2xbL8gEA
2NenoDwxr8aWnlhajSJZz8UYNkzJNJQCPG2cukPNf3YA/RYhzCxJMkMYJ3DXtiUh
UqZBMYWFftpFkh5E5FGqs7kO
=Rt6r
-----END PGP PUBLIC KEY BLOCK-----"#;

        let result = parse_key_metadata(key_data);
        assert!(result.is_ok());

        let (fingerprint, user_ids) = result.unwrap();
        assert_eq!(
            fingerprint.to_uppercase(),
            "7607A01349161EA59DC551A654F610003149BA6E"
        );
        // This test key has "Test 1" as user ID but no email, so user_ids should be empty
        assert!(user_ids.is_empty());
    }

    #[test]
    fn test_parse_key_with_email() {
        // A test key with an email address
        let key_data = r#"-----BEGIN PGP PUBLIC KEY BLOCK-----

mDMEZqWp9BYJKwYBBAHaRw8BAQdAsHf0MhUvIVpSFsEZvQnnF3IXw2lODfCU8naR
U4juKjW0IVRlc3QgVXNlciA8dGVzdC51c2VyQGV4YW1wbGUuY29tPoiZBBMWCgBB
FiEEJ0o1v8rRKmkqCwVhzaW9vsKi7GAFAmalqfQCGwMFCQPCZwAFCwkIBwICIgIG
FQoJCAsCBBYCAwECHgcCF4AACgkQzaW9vsKi7GA8xQD/YSHd7Wrf7RG4dNQJvbol
GMQX3J9XQFQsZhJzvF2PJQkA/A1MHSaoFIHPQ8nKMBje2WLMNan8vPJjVoGVOoUg
4Y0GuDgEZqWp9BIKKwYBBAGXVQEFAQEHQPOXyfn9OI/Ge8rqMAYiJJSKlbhHNuv6
7s9VhtKrJbclAwEIB4h+BBgWCgAmFiEEJ0o1v8rRKmkqCwVhzaW9vsKi7GAFAmal
qfQCGwwFCQPCZwAACgkQzaW9vsKi7GCGegD8CzKOL6csQ6xRGBBb7Q5P0GlJHF4v
s7jNLfTdgJ4AKEEA/i8Hj1Q4KmgyqE8lpZmqfAdof/LHDlLg4E5Ry/4CgIUN
=zUh6
-----END PGP PUBLIC KEY BLOCK-----"#;

        let result = parse_key_metadata(key_data);
        assert!(result.is_ok());

        let (fingerprint, user_ids) = result.unwrap();
        assert_eq!(
            fingerprint.to_uppercase(),
            "7AC6142889FA53D9268C3278280EB318F8281840"
        );
        assert_eq!(user_ids, vec!["test.user@example.com"]);
    }

    #[test]
    fn test_invalid_key_data() {
        let invalid_data = "This is not a PGP key";

        assert!(verify_key_fingerprint(invalid_data, "any_fingerprint").is_err());
        assert!(parse_key_metadata(invalid_data).is_err());
    }
}
