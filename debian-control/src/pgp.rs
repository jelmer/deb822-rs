//! PGP signature parsing.

/// Error during PGP signature parsing.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// Missing PGP signature.
    MissingPgpSignature,

    /// Payload missing in the signed message.
    MissingPayload,

    /// Truncated PGP signature.
    TruncatedPgpSignature,

    /// Junk after PGP signature.
    JunkAfterPgpSignature,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::MissingPgpSignature => write!(f, "missing PGP signature"),
            Error::TruncatedPgpSignature => write!(f, "truncated PGP signature"),
            Error::JunkAfterPgpSignature => write!(f, "junk after PGP signature"),
            Error::MissingPayload => write!(f, "missing payload"),
        }
    }
}

impl std::error::Error for Error {}

/// Strip a PGP signature from a signed message.
///
/// This function takes a signed message and returns the payload and the PGP signature.
/// If the input is not a signed message, the function returns the input as the payload and `None`
/// as the signature.
///
/// # Arguments
/// * `input` - The signed message.
///
/// # Errors
/// This function returns an error if the input is a signed message but the payload, PGP signature,
/// or the PGP signature metadata is missing, or if there is junk after the PGP signature.
/// The error indicates the reason for the failure.
///
/// # Returns
/// A tuple containing the payload and the PGP signature, if present.
///
/// # Examples
/// ```
/// let input = "-----BEGIN PGP SIGNED MESSAGE-----
/// Hash: SHA256
///
/// Hello, world!
/// -----BEGIN PGP SIGNATURE-----
/// iQIzBAEBCAAdFiEEpyNohvPMyq0Uiif4DphATThvodkFAmbJ6swACgkQDphATThv
/// odkUiw//VDVOwHGRVxpvyIjSvH0AMQmANOvolJ5EoCu1I5UG2x98UPiMV5oTNv1r
/// ...
/// =olY7
/// -----END PGP SIGNATURE-----
/// ";
/// let (output, signature) = debian_control::pgp::strip_pgp_signature(input).unwrap();
/// assert_eq!(output, "Hello, world!\n");
/// assert_eq!(signature.unwrap().len(), 136);
/// ```
pub fn strip_pgp_signature(input: &str) -> Result<(String, Option<String>), Error> {
    let mut lines = input.lines();
    let first_line = if let Some(line) = lines.next() {
        line
    } else {
        return Ok((input.to_string(), None));
    };
    if first_line != "-----BEGIN PGP SIGNED MESSAGE-----" {
        return Ok((input.to_string(), None));
    }

    // Read the metadata
    let mut metadata = String::new();
    loop {
        let line = lines.next().ok_or(Error::MissingPayload)?;
        if line.is_empty() {
            break;
        }
        use std::fmt::Write;
        writeln!(&mut metadata, "{}", line).unwrap();
    }

    let mut payload = String::new();
    loop {
        let line = lines.next().ok_or(Error::MissingPgpSignature)?;
        if line == "-----BEGIN PGP SIGNATURE-----" {
            break;
        }
        use std::fmt::Write;
        writeln!(&mut payload, "{}", line).unwrap();
    }

    let mut signature = String::new();
    loop {
        let line = lines.next().ok_or(Error::TruncatedPgpSignature)?;
        if line == "-----END PGP SIGNATURE-----" {
            break;
        }
        signature.push_str(line);
    }

    if let Some(_line) = lines.next() {
        return Err(Error::JunkAfterPgpSignature);
    }

    Ok((payload, Some(signature)))
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_strip_pgp_wrapper() {
        let input = include_str!("testdata/InRelease");

        let (output, signature) = super::strip_pgp_signature(input).unwrap();

        assert_eq!(
            output,
            r###"Origin: Debian
Label: Debian
Suite: experimental
Codename: rc-buggy
Changelogs: https://metadata.ftp-master.debian.org/changelogs/@CHANGEPATH@_changelog
Date: Sat, 24 Aug 2024 14:13:49 UTC
Valid-Until: Sat, 31 Aug 2024 14:13:49 UTC
NotAutomatic: yes
Acquire-By-Hash: yes
No-Support-for-Architecture-all: Packages
Architectures: all amd64 arm64 armel armhf i386 mips64el ppc64el riscv64 s390x
Components: main contrib non-free-firmware non-free
Description: Experimental packages - not released; use at your own risk.
"###
        );

        assert_eq!(
            signature.as_deref(),
            Some(
                r###"iQIzBAEBCAAdFiEEpyNohvPMyq0Uiif4DphATThvodkFAmbJ6swACgkQDphATThv
odkUiw//VDVOwHGRVxpvyIjSvH0AMQmANOvolJ5EoCu1I5UG2x98UPiMV5oTNv1r
B79A3nb+FL2toeuHUJBN3G1WNg6xeH0vD43hGcxhCgVn6NADogv8pBEpyynn1qC0
iketp6kEiHvGMpEj4JqOUEcq2Mafq2TTf9zEqYuTr8NqL9hC/pG8YqPKT3rhPdc3
/D4/0dTT7L+wqLgVTjjNFNcmKU1ywvaWLF5b0VktZ1W6xIqnZYfHyP0iMqolrGqF
+NG+igpsMuLI6JtoqoE+yKtWlaQi7pY7VB+OFroywNxEobzPgwdqz0r8pdJq8S4V
CMXJqoY18KHdVd4qbU9yGr6qkqopHdMMcpvV9X1UG5xDKUb2OrdbBYttKFGuIuuM
S6ZzM+26bztVXLzSra/w7gn7Qm1GluT+EncYrleAAgUvruCRrLFptDpHMAuKWKs5
OyNLh1ZUe/TrkmYGhehsVEBNmG+/HnzS7VKzLfpANHLAXthoEF9Lzqe0lPETa9NZ
rSF/EfQwh8omsaBDfighU46fZJwKGSWOIz69jXrQ6YV9hBI/frUDHQUkMLBwjnVo
8hvr0s6/8hHRwNlLRW3XQuwL+wiz0qyk6u6RRudglqSyN1FwIAtTsGkERWN82au2
DY6KLpnfN7/0bIueDUWCP40Dib+eW5Y0/Z536WhNbp8C/OIKeVyJAjMEAQEIAB0W
IQRMtQGQIHtHWKP3Onlu0Oe4JkPhMQUCZsnq6QAKCRBu0Oe4JkPhMUJsD/0ZTGIM
oI9bzhP6NadhiNNruxLQfq/+fVx/oJbyOJy4IaYPOE0JVeqzZv/wFL/XOVXw6Gg2
V/SHe0cT+iuwdKd+8oMEYaOHQUeU8RhAguypeTdizZef3YjIL+2n4v0mLeq/jMHO
a6Hyd09eUQrHedmcgViwQYOX/9/oqls0j3OGtyx1gmpIsmCxJtqsWNsXEcBLaNlm
xSAp5YYa5USenFAph4VlR2sG+VdJrG/wtCj8TuDtJCA4tOML3JB5zwgnzfpLMVU6
l+WFKkSzl0f/dlMUYRtRoU9ccpWpyajMs968QsOp0lKLZ5Kq98fSXqOzKriDimpv
4WSmlLRptRgKL0J/Nc1eYRVEPnu+tBsitLdip52SLrqYcbCOErtxOLMIIbbC2HiR
Q0lYYgky2TwO8bbCWhTyQIznldnSRhNE1STf5bctphNeWQE6zRFmMGyHh9pQYVNF
KkmCbzHcv6EbUOp7Q7c5D/mijN8On/h9TEYU6EbbrQ1AEc+IulXukzlaLCMKJ0Tx
XqsogWqW/nbOxTdudMn+qjd7gVsLtNIDKA42Csyac5Hwl9YDqgicyOMGBY88gocV
8fDXnyUhX5Es35AgO25Sh8CbISC29479o4/MdZXCGMIJEocjPx46Dy+hP1sIcFyp
KYQwHDLf3TLHWF9z0lvGFYSAq1H8gOwchDISGA==
=olY7
"###
                .replace('\n', "")
                .as_ref()
            )
        );
    }

    #[test]
    fn test_strip_pgp_no_pgp_signature() {
        let input = "Hello, world!";
        let (output, signature) = super::strip_pgp_signature(input).unwrap();
        assert_eq!(output, input);
        assert_eq!(signature, None);
    }

    #[test]
    fn test_strip_pgp_missing_payload() {
        let input = r###"-----BEGIN PGP SIGNED MESSAGE-----
Hash: SHA256
"###;
        let err = super::strip_pgp_signature(input).unwrap_err();
        assert_eq!(err, super::Error::MissingPayload);
    }

    #[test]
    fn test_strip_pgp_missing_pgp_signature() {
        let input = r###"-----BEGIN PGP SIGNED MESSAGE-----
Hash: SHA256

Hello, world!
"###;
        let err = super::strip_pgp_signature(input).unwrap_err();
        assert_eq!(err, super::Error::MissingPgpSignature);
    }

    #[test]
    fn test_strip_pgp_truncated_pgp_signature() {
        let input = r###"-----BEGIN PGP SIGNED MESSAGE-----
Hash: SHA256

Hello, world!

-----BEGIN PGP SIGNATURE-----
B79A3nb+FL2toeuHUJBN3G1WNg6xeH0vD43hGcxhCgVn6NADogv8pBEpyynn1qC0
"###;
        let err = super::strip_pgp_signature(input).unwrap_err();
        assert_eq!(err, super::Error::TruncatedPgpSignature);
    }

    #[test]
    fn test_strip_pgp_junk_after_pgp_signature() {
        let input = r###"-----BEGIN PGP SIGNED MESSAGE-----
Hash: SHA256

Hello, world!

-----BEGIN PGP SIGNATURE-----
B79A3nb+FL2toeuHUJBN3G1WNg6xeH0vD43hGcxhCgVn6NADogv8pBEpyynn1qC0
-----END PGP SIGNATURE-----
Junk after PGP signature
"###;
        let err = super::strip_pgp_signature(input).unwrap_err();
        assert_eq!(err, super::Error::JunkAfterPgpSignature);
    }
}
