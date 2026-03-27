mod Base64 {
    import java.util.Base64

    pub def encode(s: String): String =
        unsafe IO { Base64.getEncoder().encodeToString(s.getBytes()) }
}