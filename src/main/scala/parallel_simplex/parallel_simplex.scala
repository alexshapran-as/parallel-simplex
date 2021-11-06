import org.slf4j.LoggerFactory

package object parallel_simplex {
    type MSA = Map[String, Any]
    type MSS = Map[String, String]

    val logger = LoggerFactory.getLogger(getClass)
}
