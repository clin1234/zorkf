! Compile with -std=f2018
module internals
    use iso_fortran_env
    implicit none
    character(20), parameter :: medkit = 'first aid kit', knife = 'rusty knife',&
        robot='robot fighter', code='exit code', pills='antipsychotic pills',&
        bomb='hydrogen bomb'
    character(20), parameter :: possible_items(6) &
        = [medkit, knife, robot, code, pills, bomb]

    type Player
        integer :: health = 10, sanity = 8
        character(20) :: inventory(3) = ['', '', '']
    end type Player

    type Room
        character(:), ALLOCATABLE :: name, item
        integer :: room_number, number_of_people
        logical :: visited = .FALSE.
    end type Room

    type(Room) :: current_room
    type(Room), DIMENSION(:), ALLOCATABLE :: room_list

contains
    ! Generate integer between 1 and max, inclusive
    function rint(max) result(new_number)
        integer :: new_number
        integer, INTENT(IN) :: max
        real :: x

        call RANDOM_NUMBER(x)
        new_number = MOD(NINT(x*100), max)+1
    end function rint

    function new_Room(name) result(nr)
        character(25), INTENT(IN), OPTIONAL :: name
        type(Room) :: nr
        INTEGER :: err
        character(100) :: msg

        nr%room_number = rint(55)
        nr%number_of_people = rint(10)
        allocate(character(20) :: nr%item)
        nr%item = possible_items(rint(size(possible_items)))

        allocate(character(20) :: nr%name)
        if ( PRESENT(name) ) then
            nr%name = name
        else
            write(nr%name, '(a,i2)') 'Room ', nr%room_number
            !if (err /= 0) write(unit=error_unit, fmt=*) msg
        end if
    end function new_Room

    function use(slot, user) result(message)
        integer, INTENT(IN) :: slot
        type(Player), intent(in out) :: user
        character(:), ALLOCATABLE :: message
        integer :: i

        select case (user%inventory(slot))
        case (medkit)
            i = rint(10)
            user%health = user%health + i
            write(message, '(a,i2,a)') 'Wounds need to be closed, lest you wish&
            & to disturb the outside world of your grotesque hideousness. Well, &
            & you gain', i, ' health.'
        case (pills)
            i = rint(10)
            user%sanity = user%sanity + i
            write(message, '(a,i2,a)') 'Questionable pills save the say! Go on, &
            & try them. I promise therapeutic effects. ', i, &
            ' sanity have been restored.'
        case (knife)
            i = rint(5)
            current_room%number_of_people = current_room%number_of_people - i
            write(message, '(a,i2,a)') 'Justifying through the castle doctrine,&
            & (as if anyone would care about legal doctrines in this situatuion), &
            &you fire at other survivors in the room. In your act of rage, you &
            &killed ', i, ' people.'
        case (robot)
            current_room%number_of_people = 0
            message = 'Justifying through the castle doctrine,&
            & (as if anyone would care about legal doctrines in this situatuion), &
            &you let your companion rip and tear at everyone else. In the end, &
            &nothing remained but mangled metallic and electronic parts and &
            &torn-up bloody flesh mixed with fabrics.'
        case (code)
            i = rint(8)
            if ( i < 5 ) then
                message = 'Desperate to escape, huh? Try another room.'
                goto 16
            else
                message = 'By a miracle, the smudged scribblings were legible &
                &enough to open the hatch to reveal the outside world. At last,&
                & you are free. For now...'
                call exit
            end if
        case (bomb)
            message = 'In your final act of frenzied lashing against the cursed&
            & world, you allow the h    ydrogen bomb, who (surprise!), being sentient,&
            & to detonate itself. In an ironic sendoff, all that remained of the&
            & shelter is scattered concrete slabs strewn all over.'
            call exit
        end select
        user%inventory(slot) = ''
        16 continue
    end function use

    subroutine exit
        integer :: err

        if (allocated(room_list)) deallocate(room_list, stat=err)
        if (err /= 0) print *, "room_list: Deallocation request denied"
        stop
    end subroutine exit

end module internals
