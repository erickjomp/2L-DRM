module run2LDRM
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, run2LDRM!"
  end subroutine say_hello
end module run2LDRM
