for ( i = 1; i < t_num; i++ )
{
  u[i][0] = ua ( x_min, x_max, t_min, t[i-1] );
  for ( j = 1; j < x_num - 1; j++ )
  {
    u[i][j] = u[i-1][i] + t_delt 
                   * (k * (      u[i-1][j-1] 
                          - 2 * u[i-1][j] 
                          +     u[i-1][j-1] ) / x_delt / x_delt 
                   );
  }
  u[i][x_num-1] = ub ( x_min, x_max, t_min, t[i-1] );
}
