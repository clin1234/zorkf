! Compile with -std=f2018
program zorkf
    use iso_fortran_env
    use internals
    implicit none
    ! Scratch variables
    character(30) :: line = 'Test'
    !character(150) :: message
    logical :: yes, asked = .FALSE.
    integer :: i, a
    real :: r
    type(Player) :: you

    ! Preparation
    print*, 'How many rooms do you want?'
    read'(i2)', a
    ALLOCATE(room_list(a))
    do i = 1, SIZE(room_list)
        room_list(i) = new_Room()
    end do
    current_room = room_list(rint(size(room_list)))
    current_room%visited = .TRUE.
    print*, 'You are in a bomb shelter, with no way out at the moment. Have fun&
    & exploring!'
    print*
    print*, 'You are in ', current_room%name
    you = Player()
    do while (you%health > 0 .and. you%sanity > 0)
        print*, 'What would you like to do?'
        read'(a)', line
        call handle_input(line)
    end do
    if (you%health <= 0) then
        print*, 'Breathing in stale air with questionable quality was not the &
        & brightest idea, was it? Or was it picking fights with too many deranged&
        & survivors in your condition?'
        call exit()
    end if
    if (you%sanity <= 0) then
        print*, 'Your childhood claustrophobia has finally caught up to you. &
        & The enclosed spaces, the intermittent flickering of decades-old &
        & lighting, and your desperate yearn to attain natural sunlight has &
        & finnaly snapped your mind. Only psychoses lie ahead...'
        call exit()
    end if

contains
    subroutine handle_input(input)
        character(len=*), INTENT(IN) :: input

        select case (TRIM(input))
        case ('Help')
            print*, 'You are conversing through a computer terminal. Speifically,&
            & a VT100-compatible terminal version connecting to a Gentoo Linux &
            &system. Documentation is available, but quite lacking on the bomb &
            &shelter. And don''t imagine this conversation as a vain request for&
            & a genie!'
        case ('Meditate')
            print*, 'You insert into a casette player, for some reason, a song &
            &to sooth yourself within the hellspace known as the bomb shelter.'
            print*
            CALL RANDOM_NUMBER(r)
            a = rint(7)
            if ( r < 0.6 ) then
                print'(a,i2,a)', 'Normalcy, a pleasant, yet temporary reward. Well, at &
                &least you gain ', a, ' sanity.'
                you%sanity = you%sanity + a
            else
                print'(a,i2,a)', 'Somehow the song you inserted unsettled you to&
                & the point of feeling more paranoid. Frankly, you deserve it, and&
                & you lose ', a, ' sanity.'
                you%sanity = you%sanity - a
            end if
        case ('Room list')
            print'(a8,a8)', 'Rooms', 'Visited'
            do i = 1, size(room_list)
                print'(a8,l2)', room_list(i)%name, room_list(i)%visited
            end do
        case ('Status')
            print*, 'Health: ', you%health
            print*, 'Sanity: ', you%sanity
            print*
            do i = 1, size(you%inventory)
                print*, 'Slot ', i, ': ', you%inventory(i)
            end do
        case ('Explore')
            print'(a,i2,a,a)', 'This room has ', current_room%number_of_people,&
                ' people and ', current_room%item
        case ('Simulation')
            if ( asked .eqv. .FALSE. ) then
                print*, 'You are not in a simulation.'
                asked = .TRUE.
                you%sanity = you%sanity - 1
            else
                print*, 'This clone of Zork was compiled using ', compiler_version(),&
                ' arguments ', compiler_options()
                you%sanity = you%sanity - rint(6)
            end if
        case ('Contemplate')
            print*, '"Surely there must be a better approach to escape this &
            & hellscape, is there?" you ponder. Perhaps rip-and-tear like &
            &Doomguy while in Hell, seeking an exit portal to Mars? Right.....?'
            print*
            call RANDOM_NUMBER(r)
            if ( r < 0.55 ) then
                print*, 'An openly-obvious answer that anyone in your situation, &
                & even a simpleton, can conclude. Yes. And so you move on.'
            else
                a = rint(9)
                print*, 'Ah, the recurring naivety and blinding stupidity within&
                & each human being, especially you. To even think that you, given&
                & your less-than-stellar record of navigating within dark enclosed&
                & spaces speaks much to your foolhardy bravado. For daring to &
                & conclude otherwise, I hereby punish you with further insanity!'
                print*
                print*, 'You lost ', a, ' sanity.'
                print*, 'And don''t even try to ask yourself that ever again!'
                you%sanity = you%sanity - a
            end if
        case ('Go to')
            print*, 'Which room? Enter a integer.'
            read*, a
            do i = 1, SIZE(room_list)
                if ( room_list(i)%room_number == a ) then
                    print*, 'You have gone to ', room_list(i)%name
                    room_list(i)%visited = .TRUE.
                    current_room = room_list(i)
                    goto 1
                end if
            end do
            print*, 'Did your guardian or parent dropped you too high and too often&
            & as a child?'
            1 continue
        case ('Talk')
            call RANDOM_NUMBER(r)
            if ( r < 0.6 ) then
                a = rint(12)
                print'(a,i2,a)', 'The people you encounter are rather trigger-happy, &
                &aren''t they? You now take ', a, ' damage.'
                you%health = you%health - a
            else
                print*, 'To your surprise, the people you encounter are rather &
                &amicable, sharing your plight. After a few moments, you move on.'
            end if
            if ( current_room%number_of_people == 0 ) then
                print*, 'This reminds me of an old adage: an idle and lone &
                &babbler gurantees an early death.'
                you%sanity = you%sanity - rint(5)
            end if
        case ('Exit')
            print*, 'Are you really sure? Your own senses confirm there is no &
            &escape, so stop trying. Type either T or F.'
            read'(l1)', yes
            if ( yes ) call exit
        case ('Pick up item')
            if ( current_room%item /= '' ) then
                print*, 'You picked up ', current_room%item
                print*, 'Your agility in using them, is most certainly questionable&
                &...'
                do i = 1, size(you%inventory)
                    if ( you%inventory(i) == '' ) then
                        you%inventory(i) = current_room%item
                        current_room%item = ''
                        goto 2
                    end if
                end do
                if(all(you%inventory /= '', dim=1)) then
                        print*, 'Item overflow. Do be a dear and drop your item &
                        &that you are least able to use effectively, please.'
                        goto 2
                end if
            else
                print*, 'No item available. Chasing phantom objects again?'
            end if
            2 continue
        case ('1', '2', '3')
            write(a, '(a)') input
            if (you%inventory(a) == '') then
                print*, 'I see. You are attempting to &
            &deliver a incorporeal object to this room, or deliver one from a&
            & different universe. Unfortunately, such requests are invalid per &
            & the Penal Code of the USSR, Title XV. Criminal sentences range from&
            & six to fourteen months prison. I suggest that you don not attempt&
            & this stunt before outside authorities find you.'
                print*
                print*, 'Translation: try something else. For your sake.'
                go to 19
            endif

            print*, 'drop or use? (Type either word)'
            read*, line
            select case (trim(line))
                case ('drop')
                    print'(a,a,a)', 'You dropped ', you%inventory(a), '. Beware.'
                    current_room%item = you%inventory(a)
                    you%inventory(a) = ''
                case ('use')
                    print*, use(a, you)
                case default
                    print*, 'Your imbecility is astounding... A simple binary request.&
                    & How badly can you possibly screw something painfully simple?'
            end select
            19 continue
        case default
            write(error_unit, '(a)') 'Invalid command. What are you, a sorcerer?'
            you%sanity = you%sanity - 1
        end select
    end subroutine handle_input
end program zorkf
