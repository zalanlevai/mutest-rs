//@ build
//@ stderr: empty

#![feature(stdarch_arm_feature_detection)]

#[test]
fn test() {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    let _sse2 = is_x86_feature_detected!("sse2");
    #[cfg(target_arch = "arm")]
    let _aes = std::arch::is_arm_feature_detected!("aes");
    #[cfg(target_arch = "aarch64")]
    let _aes = std::arch::is_aarch64_feature_detected!("aes");
}
