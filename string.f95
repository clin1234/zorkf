module string

  implicit none
contains
  function matches(string1, string2) result(answer)
      implicit none
      character(len=*), intent(in) :: string1
      character(len=*), intent(in) :: string2
      character(len=:),allocatable :: temp
      integer add
      logical answer
      character(len=*), parameter :: fmt= '(A6,X,I0)'

      answer = .FALSE.
      add = 0
      if(len(string2) > len(string1)) return
      answer = index(string1, string2)
      if(answer==0) return
      !     Print the location of the match for part 2
      write(*,fmt) " at ", answer
      !     Handle multiple occurrences of a string for part 2.
      add = answer
      temp = string1(answer+1:)
      do while(answer>0)
         answer = index(temp, string2)
         add = add + answer
         if(answer>0) write(*,fmt) " at ", add
         !          deallocate(temp)
         temp = string1(add+1:) ! auto reallocation
      end do
      answer = .TRUE.
   end function matches

end module string
