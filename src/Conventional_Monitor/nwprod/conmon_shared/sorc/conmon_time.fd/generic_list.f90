module generic_list

  use data

  implicit none

  private
  public :: list_node_t
  public :: list_init, list_free
  public :: list_insert, list_put, list_get, list_next

  ! Linked list node
  type :: list_node_t
     private
     type(data_ptr) :: data
     type(list_node_t), pointer :: next => null()
  end type list_node_t


contains

  !-----------------------------------------------------------------------
  ! Initialize a head node SELF and optionally store the provided DATA.
  !
  subroutine list_init(self, data)
    type(list_node_t), pointer :: self
    type(data_ptr), intent(in), optional :: data

    allocate(self)
    nullify(self%next)

    if (present(data)) then
       self%data = data
    else
       nullify(self%data%p)
    end if
  end subroutine list_init


  !--------------------------------------------------------
  ! Free the entire list and all data, beginning at SELF
  !
  subroutine list_free(self)
    type(list_node_t), pointer :: self
    type(list_node_t), pointer :: current
    type(list_node_t), pointer :: next

    current => self
    do while (associated(current))
       next => current%next
       if (associated(current%data%p)) then
          deallocate(current%data%p)
          nullify(current%data%p)
       end if
       deallocate(current)
       nullify(current)
       current => next
    end do
  end subroutine list_free


  !------------------------------------------------------------
  ! Insert a list node after SELF containing DATA (optional)
  !
  subroutine list_insert(self, data)
    type(list_node_t), pointer :: self
    type(data_ptr), intent(in), optional :: data
    type(list_node_t), pointer :: next

    allocate(next)
    nullify(next%next)

    if (present(data)) then
       next%data = data
    else
       nullify(next%data%p)
    end if

    next%next => self%next
    self%next => next
  end subroutine list_insert


  !-------------------------------------------
  ! Store DATA in list node SELF
  !
  subroutine list_put(self, data)
    type(list_node_t), pointer :: self
    type(data_ptr), intent(in) :: data

    if (associated(self%data%p)) then
       deallocate(self%data%p)
       nullify(self%data%p)
    end if
    self%data = data
  end subroutine list_put


  !---------------------------------------------
  ! Return the DATA stored in the node SELF
  !
  function list_get(self) result(data)
    type(list_node_t), pointer :: self
    type(data_ptr) :: data
    data = self%data
  end function list_get


  !---------------------------------------------
  ! Return the next node after SELF
  !
  function list_next(self)
    type(list_node_t), pointer :: self
    type(list_node_t), pointer :: list_next
    list_next => self%next
  end function list_next

end module generic_list
