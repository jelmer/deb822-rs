use crate::signature::Signature;
use sequoia_openpgp::cert::CertParser;
use sequoia_openpgp::parse::Parse;
use sequoia_openpgp::Cert;
use std::fmt;
use std::time::SystemTime;

/// Errors that can occur during key management operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeyManagementError {
    /// Failed to parse key data
    ParseError(String),
    /// No valid certificates found in key data
    NoCertificates,
    /// Key has expired
    KeyExpired,
    /// Key is not yet valid or has expired
    KeyNotValid,
    /// Key expires soon (includes days until expiration)
    KeyExpiringSoon(u64),
    /// Fingerprint mismatch
    FingerprintMismatch {
        /// Expected fingerprint
        expected: String,
        /// Actual fingerprint found
        actual: String,
    },
    /// Invalid fingerprint format
    InvalidFingerprint(String),
    /// Invalid fingerprint length
    InvalidFingerprintLength {
        /// Actual length of the fingerprint
        length: usize,
        /// Expected length description
        expected: String,
    },
}

impl fmt::Display for KeyManagementError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KeyManagementError::ParseError(msg) => write!(f, "Failed to parse key data: {}", msg),
            KeyManagementError::NoCertificates => {
                write!(f, "No valid certificates found in key data")
            }
            KeyManagementError::KeyExpired => write!(f, "Key has expired"),
            KeyManagementError::KeyNotValid => write!(f, "Key has expired or is not yet valid"),
            KeyManagementError::KeyExpiringSoon(days) => write!(f, "Key expires in {} days", days),
            KeyManagementError::FingerprintMismatch { expected, actual } => {
                write!(
                    f,
                    "Fingerprint mismatch! Expected: {}, Got: {}",
                    expected, actual
                )
            }
            KeyManagementError::InvalidFingerprint(msg) => write!(f, "{}", msg),
            KeyManagementError::InvalidFingerprintLength { length, expected } => {
                write!(
                    f,
                    "Invalid fingerprint length: {}. Expected {}",
                    length, expected
                )
            }
        }
    }
}

impl std::error::Error for KeyManagementError {}

/// Check if a GPG key is expired or about to expire
pub fn check_key_expiration(cert: &Cert) -> Option<KeyManagementError> {
    use sequoia_openpgp::policy::StandardPolicy;

    let policy = StandardPolicy::new();
    let now = SystemTime::now();

    // Check if the certificate is alive according to the policy
    match cert.with_policy(&policy, now) {
        Ok(valid_cert) => {
            // Check if primary key has expiration
            if let Some(expiration) = valid_cert.primary_key().key_expiration_time() {
                if expiration <= now {
                    return Some(KeyManagementError::KeyExpired);
                }

                // Warn if expiring within 30 days
                if let Ok(duration_until) = expiration.duration_since(now) {
                    let days_until = duration_until.as_secs() / 86400;
                    if days_until < 30 {
                        return Some(KeyManagementError::KeyExpiringSoon(days_until));
                    }
                }
            }
            None
        }
        Err(_) => {
            // Certificate or one of its components has expired or is not valid
            Some(KeyManagementError::KeyNotValid)
        }
    }
}

/// Verify that a key's fingerprint matches the expected fingerprint
pub fn verify_key_fingerprint(
    key_data: &str,
    expected_fingerprint: &str,
) -> Result<(), KeyManagementError> {
    let parser = CertParser::from_bytes(key_data.as_bytes())
        .map_err(|e| KeyManagementError::ParseError(e.to_string()))?;

    let certs: Result<Vec<Cert>, _> = parser.collect();
    let certs = certs.map_err(|e| KeyManagementError::ParseError(e.to_string()))?;

    if certs.is_empty() {
        return Err(KeyManagementError::NoCertificates);
    }

    let cert = &certs[0];
    let fingerprint = cert.fingerprint().to_hex();

    // Normalize fingerprints for comparison (remove spaces and make uppercase)
    let normalized_actual = fingerprint.replace(" ", "").to_uppercase();
    let normalized_expected = expected_fingerprint.replace(" ", "").to_uppercase();

    if normalized_actual != normalized_expected {
        return Err(KeyManagementError::FingerprintMismatch {
            expected: expected_fingerprint.to_string(),
            actual: fingerprint,
        });
    }

    Ok(())
}

/// Create an inline signature from GPG key data
pub fn create_inline_signature(key_data: &str) -> Result<Signature, KeyManagementError> {
    // First, let's parse the key to validate it
    let parser = CertParser::from_bytes(key_data.as_bytes())
        .map_err(|e| KeyManagementError::ParseError(e.to_string()))?;

    let certs: Result<Vec<Cert>, _> = parser.collect();
    let certs = certs.map_err(|e| KeyManagementError::ParseError(e.to_string()))?;

    if certs.is_empty() {
        return Err(KeyManagementError::NoCertificates);
    }

    // Check for expiration
    if let Some(error) = check_key_expiration(&certs[0]) {
        eprintln!("Warning: {}", error);
    }

    // Create inline signature with the key data (using KeyBlock variant)
    Ok(Signature::KeyBlock(key_data.to_string()))
}

/// Validate a GPG fingerprint format
pub fn validate_fingerprint(fingerprint: &str) -> Result<String, KeyManagementError> {
    // Remove spaces and convert to uppercase
    let cleaned = fingerprint.replace(" ", "").to_uppercase();

    // Check if it's a valid hex string
    if !cleaned.chars().all(|c| c.is_ascii_hexdigit()) {
        return Err(KeyManagementError::InvalidFingerprint(
            "Fingerprint must contain only hexadecimal characters".to_string(),
        ));
    }

    // GPG fingerprints should be 40 characters (160 bits) for SHA-1
    // or 64 characters (256 bits) for SHA-256
    if cleaned.len() != 40 && cleaned.len() != 64 {
        return Err(KeyManagementError::InvalidFingerprintLength {
            length: cleaned.len(),
            expected: "40 or 64 characters".to_string(),
        });
    }

    Ok(cleaned)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_fingerprint() {
        // Valid SHA-1 fingerprint
        assert!(validate_fingerprint("1234567890ABCDEF1234567890ABCDEF12345678").is_ok());

        // Valid with spaces
        assert!(validate_fingerprint("1234 5678 90AB CDEF 1234 5678 90AB CDEF 1234 5678").is_ok());

        // Invalid characters
        assert!(validate_fingerprint("XXXX567890ABCDEF1234567890ABCDEF12345678").is_err());

        // Invalid length
        assert!(validate_fingerprint("1234567890ABCDEF").is_err());
    }
}
