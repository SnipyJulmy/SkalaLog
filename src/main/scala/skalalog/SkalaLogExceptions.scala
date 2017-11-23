package skalalog


class ParserException(private val message: String = "",
                      private val cause: Throwable = None.orNull
                     ) extends Exception(message, cause)

class NoListFoundException(private val message: String = "",
                           private val cause: Throwable = None.orNull
                          ) extends ParserException

class NoFunctorException(private val message: String = "",
                         private val cause: Throwable = None.orNull
                        ) extends Exception(message,cause)
