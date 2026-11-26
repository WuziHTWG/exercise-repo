package functions

class FunctionsTests extends munit.FunSuite:
    test("validMatrikel accepts a numeric value"):
        assertEquals(validMatrikel("12345"), Some("12345"))

    test("validMatrikel rejects non-numeric input"):
        assertEquals(validMatrikel("abc12"), None)

    test("validEmail accepts an address containing at"):
        assertEquals(validEmail("max@example.com"), Some("max@example.com"))

    test("validEmail rejects input without at"):
        assertEquals(validEmail("max.example.com"), None)

    test("parseLine parses and trims three fields"):
        val parsed = parseLine("  12345 , max@example.com , 2024-10-01 ")
        assert(parsed.isSuccess)
        assertEquals(
          parsed.toOption.map(input => (input.matrikel, input.email, input.enrolled)),
          Some(("12345", "max@example.com", "2024-10-01"))
        )

    test("parseLine fails when the input does not have three fields"):
        assert(parseLine("12345,max@example.com").isFailure)
