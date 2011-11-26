allocate ( u(1:x_num,1:t_num) )
call u0 ( x_min, x_max, t_min, x_num, x, u(1:x_num,1) )
do j = 2, t_num
  call ua ( x_min, x_max, t_min, t(j-1), u(1,j) )
  u(2:x_num-1,j) = u(2:x_num-1,j-1) &
                 + k * (      u(1:x_num-2,j-1) &
                        - 2 * u(2:x_num-1,j-1) &
                        +     u(3:x_num,  j-1) ) &
		     / x_delt / x_delt
  call ub ( x_min, x_max, t_min, t(j-1), u(x_num,j) )
end do
