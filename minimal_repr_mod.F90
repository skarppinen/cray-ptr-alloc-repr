module minimal_repr_mod
    implicit none
    integer, parameter :: ik = 8
    integer, parameter :: rk = 8

contains

subroutine driver()
    integer(kind = ik), parameter :: arrlen = 8_ik
    integer(kind = ik), parameter :: nblocks = 16_ik
    integer(kind = ik) :: block_id
#ifdef USE_ALLOCATABLE
    real(kind = rk), allocatable :: stack(:, :)
#else
    real(kind = rk) :: stack(arrlen, nblocks)
#endif
    real(kind = rk) :: output(arrlen, nblocks)
    real(kind = rk) :: x(arrlen)

    output = 0.0_rk
    x = 3.0_rk
#ifdef USE_ALLOCATABLE
    allocate(stack(arrlen, nblocks))
#endif

    !$acc data copyin(x) copyout(output) create(stack)
    !$acc parallel loop
    do block_id = 1_ik, nblocks
        call kernel(arrlen, x, output(:, block_id), stack(:, block_id))
    end do
    !$acc end parallel loop
    !$acc end data
    do block_id = 1_ik, nblocks
        print *, "Sum of column ", block_id, " is: ", sum(output(:, block_id))
    end do
#ifdef USE_ALLOCATABLE
    deallocate(stack)
#endif
end subroutine driver

subroutine kernel(arrlen, x, output, stack)
    integer(kind = ik), intent(in) :: arrlen
    real(kind = rk), intent(in) :: x(arrlen)
    real(kind = rk), intent(inout) :: output(arrlen)
    real(kind = rk), contiguous, intent(inout) :: stack(:)

    real(kind = rk) :: tmpA(arrlen)
    integer(kind = ik) :: i
    pointer(ptrA, tmpA)
    !$acc routine vector
    !$acc data present(stack, x, output)

    ! Setup Cray pointer. 
    ptrA = loc(stack(1_ik)) ! `tmpA` now points to beginning of stack.

    !$acc loop vector
    do i = 1_ik, arrlen
        tmpA(i) = x(i)
    end do

    !$acc loop vector
    do i = 1_ik, arrlen
        output(i) = tmpA(i)
    end do
    !$acc end data

end subroutine kernel

end module minimal_repr_mod
