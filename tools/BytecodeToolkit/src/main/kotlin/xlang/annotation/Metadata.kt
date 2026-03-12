package xlang.annotation


@SuppressWarnings("unchecked")
@Target(AnnotationTarget.FIELD, AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.BINARY)
annotation class Metadata(val value: String)
