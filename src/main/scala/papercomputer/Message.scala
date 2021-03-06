package papercomputer

sealed trait Message
case object MinRegisterValueMustBeLessOrEqualZero extends Message
case object MaxRegisterValueMustBeGreaterOrEqualZero extends Message
case object RegisterValueMustNotBeSmallerThanMinRegisterValue extends Message
case object RegisterValueMustNotBeGreaterThanMaxRegisterValue extends Message
case object IllegalAccessToNonExistingRegisterNumber extends Message
case object IllegalReferenceToNonExistingLineNumber extends Message
case object NoNextLinenNumberFoundInProgram extends Message
case object CannotRunAFinishedProgram extends Message
case object StartLineNotFoundInProgram extends Message
case object MessageDuringUnitTests extends Message
case object IllegalState extends Message
