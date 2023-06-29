PROGRAM stm

  ! Simple temperature model for stored slurry 
  ! By Sasha D. Hafner
  ! See https://github.com/sashahafner/STM for latest version and more details

  IMPLICIT NONE
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Declaration statements 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Days, and other integers
  INTEGER :: TS          ! Time step
  INTEGER :: DOY, DOYprev         ! Day of year (1 - 365)
  INTEGER :: DOS         ! Day of simulation
  INTEGER :: YR          ! Relative year (1 + )
  INTEGER :: nDays       ! Number of days in simulation
  INTEGER :: startingDOY ! Starting day of year
  INTEGER :: hottestDOY  ! Hottest day of year
  INTEGER :: raddestDOY  ! Day of year with most radiation
  INTEGER :: emptyDOY1   ! 
  INTEGER :: emptyDOY2   ! 
  INTEGER :: wallAvePeriod, floorAvePeriod ! Number of days in running averages for wall and floor temperature
  INTEGER :: fileStat    ! End of file indicator
  INTEGER :: fileRow     ! File row to check for problem

  ! Discretization
  INTEGER :: Z, R
  INTEGER :: nz, nr, nt 
  REAL :: dz, dr, dt
  REAL, DIMENSION(20) :: horArea, verArea, cr
  REAL :: innerArea
  REAL, DIMENSION(20, 20) :: cellVol        !

  ! Simulation ID
  CHARACTER (LEN=10) :: ID ! ID code of simulation
  !!CHARACTER (LEN=1) :: ventType ! Type of ventilation

  ! File names
  CHARACTER (LEN=30) :: userParFile, parFile, weatherFile, levelFile

  ! Version string
  CHARACTER (LEN=45) :: verString = 'Starting STM version XXX (28 June 2023)'

  ! Command line arguments, length
  INTEGER :: numArgs

  ! Temperatures, all in degrees C
  REAL, DIMENSION(366) :: tempAir, tempFloor, tempWall ! Air, floor (bottom of store or pit), and wall (side of store or pit)
  REAL :: tempAirSum, tempAirAve ! Average air temperature
  REAL :: dTemp          ! Change in temperature during time step (deg. C) 
  REAL :: maxAnnTemp     ! Maximum daily average air temperature over the year
  REAL :: minAnnTemp     ! Minimum daily average air temperature over the year
  REAL :: tempInitial    ! Initial slurry temperature
  REAL, DIMENSION(20,20) :: temp ! Hourly slurry temperature 
  REAL :: trigPartTemp   ! Sine part of temperature expression (intermediate in calculation)

  ! Sums and averages
  REAL :: retentionTime

  ! Geometry of storage structure
  REAL :: depth          ! Slurry depth (m)
  REAL :: diameter       ! Length and width of store/pit (m)

  ! Solar
  REAL :: absorp         ! Slurry or cover effective absorptivity (dimensionless)
  REAL :: maxAnnRad      ! Maximum daily average radiation over the year
  REAL :: minAnnRad      ! Minimum daily average radiation over the year
  REAL, DIMENSION(366) :: solRad ! Average solar radiation rate (W/m2)
  REAL :: trigPartRad    ! Sine part of radiation expression (intermediate in calculation)
  
  ! Other slurry variables
  REAL :: slurryMassTot, slurryMassTotInit, massFrozen = 0   ! Slurry mass (Mg = 1000 kg = metric tonnes)
  REAL :: slurryVol      ! Initial slurry volume (m3) NTS not consistent name
  REAL, DIMENSION(20,20) :: cellMass ! Slurry in cell (kg)

  ! Heat transfer coefficients and related variables
  REAL :: Rair, Rwall, Rfloor, Rsoil ! User-entered resistance terms K-m2/W
  REAL :: kConv          ! Slurry thermal conductivity
  REAL :: Rewall, Rbottom, Rtop ! Calculated resistance K-m2/W 
  REAL :: cpSlurry             ! Heat capacity of slurry J/kg-K
  REAL :: cpLiquid, cpFrozen  ! Heat capacity of liquid or frozen slurry J/kg-K
  REAL :: tempFreeze     ! Freezing point of slurry (degrees C)
  REAL :: hfSlurry       ! Latent heat of fusion of slurry J/kg
  REAL :: dSlurry        ! Density of slurry kg/m3
  REAL :: soilConstDepth ! Soil depth where averaging period reaches 1 full yr, temperature (virtually) constant, in m
  REAL :: soilOffset     ! Offset for calculating soil temperature, use air temperature + soilOffset, in deg. C
  REAL :: heatGen        ! Slurry heat generation term W/m3 

  ! Heat flow total in J
  REAL :: HH              ! Total heat flow out of slurry in a time step
  REAL :: HHadj           ! Total adjusted for melting slurry in J
  REAL :: sumHH, sumHHadj ! Sum for average

  ! Heat flow variables in W out of slurry
  REAL, DIMENSION(20,20) :: Qrad           ! "To" sun
  REAL, DIMENSION(20,20) :: Qslur2air      ! To air
  REAL, DIMENSION(20,20) :: Qslur2wall     ! Out through wall (to air)
  REAL, DIMENSION(20,20) :: Qslur2floor    ! Out through floor to soil
  REAL, DIMENSION(20,20) :: Qgen           ! Heat generation
  REAL, DIMENSION(20,20) :: Qconv          ! Convection
  REAL, DIMENSION(20,20) :: Qout           ! Total
  REAL, DIMENSION(20,20) :: QoutPart       ! Total after some use for melting/freezing
  REAL, DIMENSION(20,20) :: Qtot
  REAL, DIMENSION(20,20) :: sumQout        ! Sum of total for average

  LOGICAL :: calcWeather     ! .TRUE. when weather inputs are calculated (otherwise read from file)
  LOGICAL :: fixedFill       ! .TRUE. when slurry is added at a fixed rate, specified in user par file
  LOGICAL :: useSS           ! .TRUE. when steady-state temperature was used at some point within a day
  LOGICAL :: constantTempIn  ! .TRUE. when temperature of added slurry is constant and added slurry brings heat energy in
  LOGICAL :: airTempIn       ! .TRUE. when temperature of added slurry is set to air temperature

  CHARACTER (LEN = 3) tempInSetting

  ! Other parameters
  REAL, PARAMETER :: PI = 3.1415927
  REAL, PARAMETER :: soilFreezeDiv = 1.0  ! Fudge factor for soil freezing (1.0 means it has no effect)

  ! Timer
  INTEGER :: ttbeginning = 0, ttend = 0, ttrate = 0

  ! Date and time
  INTEGER, DIMENSION(8) :: ttt 
  CHARACTER (LEN = 10) :: date

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Get file names from call
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  numArgs = COMMAND_ARGUMENT_COUNT()
  IF (numArgs .EQ. 3) THEN
    CALL GET_COMMAND_ARGUMENT(1, ID)
    CALL GET_COMMAND_ARGUMENT(2, parFile)
    CALL GET_COMMAND_ARGUMENT(3, userParFile)
    calcWeather = .TRUE.
    fixedFill = .TRUE.
  ELSE IF (numArgs .EQ. 4) THEN
    CALL GET_COMMAND_ARGUMENT(1, ID)
    CALL GET_COMMAND_ARGUMENT(2, parFile)
    CALL GET_COMMAND_ARGUMENT(3, userParFile)
    CALL GET_COMMAND_ARGUMENT(4, weatherFile)
    calcWeather = .FALSE.
    fixedFill = .TRUE.
  ELSE
    WRITE(*,*) 'stm help'
    WRITE(*,*) 'Usage: stm[.exe] ID parfile userparfile [weatherfile] [levelfile]'
    WRITE(*,*) 'Use stm.exe on Windows.'
    WRITE(*,*) 'Weather file and level file are optional.'
    WRITE(*,*) 'See https://github.com/sashahafner/STM-applications for examples.'
    STOP
  END IF

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Open files
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Input files
  OPEN (UNIT=1, FILE=userParFile, STATUS='OLD')
  OPEN (UNIT=2, FILE=parFile, STATUS='OLD')
  IF (.NOT. calcWeather) THEN
    OPEN (UNIT=3, FILE=weatherFile, STATUS='OLD')
  END IF

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Output and log files
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Output files, name based on ID
  OPEN (UNIT=10,FILE=(''//TRIM(ID)//'_temp.csv'), STATUS='UNKNOWN')
  OPEN (UNIT=11,FILE=(''//TRIM(ID)//'_area.csv'), STATUS='UNKNOWN')
  OPEN (UNIT=12,FILE=(''//TRIM(ID)//'_mass.csv'), STATUS='UNKNOWN')
  !OPEN (UNIT=12,FILE=(''//TRIM(ID)//'_weather.csv'), STATUS='UNKNOWN')

  ! Log file, name based on ID
  OPEN (UNIT=20,FILE=(''//TRIM(ID)//'_log.txt'), STATUS='UNKNOWN')

  ! Start timer
  CALL SYSTEM_CLOCK(ttbeginning, ttrate)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Start log file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  WRITE(*,'(A)') verString 
  WRITE(20,'(A)') verString 
  CALL DATE_AND_TIME(DATE = date, VALUES = ttt)
  WRITE(20,'(A, I4, 5(A, I2.2))') 'Date and time: ', ttt(1), '/', ttt(2), '/', ttt(3), ' ', ttt(5), ':', ttt(6), ':', ttt(7)
  WRITE(20,'(A)') 
  WRITE(20,'(2A)') 'Simulation ID: ', TRIM(ID)
  WRITE(20,'(2A)') 'User par file: ', userParFile
  WRITE(20,'(2A)') 'Par file: ', parFile
  IF (calcWeather) THEN
    WRITE(20,'(A)') 'Weather is calculated (no input file)'
  ELSE 
    WRITE(20,'(2A)') 'Weather file: ', weatherFile
  END IF
  IF (fixedFill) THEN
    WRITE(20,'(A)') 'Slurry level is calculated (no input file)'
  ELSE 
    WRITE(20,'(2A)') 'Level file: ', levelFile
  END IF
  WRITE(20,*) 

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Read parameters
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! User settings
  READ(1,*)
  READ(1,*) nDays
  READ(1,*) startingDOY
  !READ(1,*) nStores
  READ(1,*) depth
  READ(1,*) diameter
  READ(1,*) tempInitial
  READ(1,*) nz
  READ(1,*) nr
  READ(1,*) dt

  IF (calcWeather) THEN
    READ(1,*)
    READ(1,*)
    READ(1,*)
    READ(1,*)
    READ(1,*) minAnnTemp, maxAnnTemp, hottestDOY
    READ(1,*)
    READ(1,*)
    READ(1,*) minAnnRad, maxAnnRad, raddestDOY
  END IF

  ! Other parameters
  READ(2,*) 
  READ(2,*) 
  READ(2,*) 
  READ(2,*) 
  READ(2,*) 
  READ(2,*) dSlurry, cpLiquid, cpFrozen, hfSlurry, tempFreeze
  READ(2,*) 
  READ(2,*) 
  READ(2,*) kConv
  READ(2,*) 
  READ(2,*) 
  READ(2,*) Rair, Rwall, Rfloor, Rsoil
  READ(2,*) 
  READ(2,*) 
  READ(2,*) 
  READ(2,*) absorp, soilConstDepth, soilOffset, heatGen

  ! Output file header
  WRITE(10,"(A)") 'Day of sim.,Day of year,Year,Z index,R index,Z position,R position,Air T,Floor T,Slurry T'
  WRITE(10,"(A)") ',,,,,(m),(m),(deg. C),(deg. C),(deg. C)'
  WRITE(10,"(A)") 'day,doy,year,z,r,zm,rm,air_temp,floor_temp,slurry_temp,rad,air,floor,wall,conv,gen,tot'

  !WRITE(11,"(A)") 'Day of sim.,Day of year,Year,Radiation,Generation,Air,Floor,Lower wall,Upper wall,Feed,Total,Total step,&
  !  &          Total step adjusted,Steady state temp,Steady state used'
  !WRITE(11,"(A)") ',,,(W),(W),(W),(W),(W),(W),(W),(W),(J),(J),,'
  !WRITE(11,"(A)") 'day,doy,year,rad,gen,air,floor,lower_wall,upper_wall,feed,total,total_step,total_adjusted,&
  !  &          steady_state_used,steady_state_temp'

  !WRITE(12,"(A)") 'Day of sim.,Day of year,Year,Air T,Radiation'
  !WRITE(12,"(A)") ',,,(deg. C),(W/m2)'
  !WRITE(12,"(A)") 'day,doy,year,air_temp,rad'

  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Initial calculations
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !slurryMassTot = slurryVol * dSlurry / 1000. ! Slurry mass is in metric tonnes = Mg = 1000 kg

  ! Determine tempAir, solRad, and soil temperatures for each day within a complete year
  ! Start day loop
  IF (calcWeather) THEN
    DO DOY = 1,365,1
      ! Use sine curve to determine air temp
      trigPartTemp = (maxAnnTemp - minAnnTemp)*SIN((DOY - hottestDOY)*2*PI/365. + 0.5*PI)/2.
      tempAir(DOY) = trigPartTemp + (minAnnTemp + maxAnnTemp)/2.
      trigPartRad = (maxAnnRad - minAnnRad)*SIN((DOY - raddestDOY)*2*PI/365. + 0.5*PI)/2.
      solRad(DOY) = trigPartRad + (minAnnRad + maxAnnRad)/2.
      IF (solRad(DOY) < 0) THEN
        solRad(DOY) = 0
      END IF
    END DO
    tempAirAve = (minAnnTemp + maxAnnTemp) / 2.
  ELSE ! Read weather data from file
    ! Skip header
    READ(3,*)
    tempAirSum = 0.
    READ(3,*,IOSTAT=fileStat) DOY, tempAir(DOY), solRad(DOY)
    fileRow = 1
    DO WHILE (.NOT. IS_IOSTAT_END(fileStat))
      IF (DOY > 365) THEN
        WRITE(20,*) "Warning: Day of year > 365 found in weather file, and will be ignored."
      ELSE 
        tempAirSum = tempAirSum + tempAir(DOY)
      END IF
      READ(3,*,IOSTAT=fileStat) DOY, tempAir(DOY), solRad(DOY)
      fileRow = fileRow + 1
      IF (fileRow > 367) THEN
        WRITE(20,*) 'Error: More than 1 year of data found in weather file. Stopping.'
        WRITE(*,*) 'Error: More than 1 year of data found in weather file. Stopping.'
        STOP
      END IF
    END DO
    tempAirAve = tempAirSum / 365.
  END IF

  ! Soil temperature based on moving average
  floorAvePeriod = 5.

  ! Then floor
  IF (floorAvePeriod .GE. 365) THEN
    tempFloor(:) = tempAirAve
  ELSE 
    ! First need to calculate value for DOY = 1
    tempFloor(1) = tempAir(1) / floorAvePeriod
    DO DOY = 365 - floorAvePeriod + 2,365,1
      tempFloor(1) = tempFloor(1) + tempAir(DOY)/floorAvePeriod
    END DO
    IF (tempFloor(1) .LT. 0.) THEN
      tempFloor(1) = tempFloor(1) / soilFreezeDiv
    END IF

    DO DOY = 2,365,1
      IF (DOY > floorAvePeriod) THEN
        tempFloor(DOY) = tempFloor(DOY - 1) - tempAir(DOY - floorAvePeriod)/floorAvePeriod + tempAir(DOY)/floorAvePeriod
      ELSE 
        tempFloor(DOY) = tempFloor(DOY - 1) - tempAir(365 + DOY - floorAvePeriod)/floorAvePeriod + tempAir(DOY)/floorAvePeriod
      END IF 
      IF (tempFloor(DOY) .LT. 0.) THEN
        tempFloor(DOY) = tempFloor(DOY) / soilFreezeDiv
      END IF
    END DO
  END IF

  ! Add soil temperature offset
  tempFloor(:) = tempFloor(:) + soilOffset
  tempWall(:) = tempWall(:) + soilOffset

  ! Set initial slurry temperature and specific heat
  temp = tempInitial
  cpSlurry = cpLiquid

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Cells
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! R = 1 is centermost ring
  ! Z = 1 is bottom layer

  ! Cylindrical geometry 
  ! R =     nr   nr-1   . . .      1
  ! Z = nz  C     X    . . .       D
  !   nz-1  X     X    . . .       X
  !         X     X    . . .       X
  !         .     .    . . .       .
  !         .     .    . . .       .
  !         .     .    . . .       .
  !     1   B     X    . . .       A R = 1
  !                            Z = 1

  ! cell dimensions
  dz = depth / nz
  dr = diameter / 2. / nr

  ! Number of time steps per day
  nt = 24. / dt

  ! Cumulative radius (to outer surface of each cell)
  cr(1) = dr
  DO R = 2,nr,1
    cr(R) = cr(R-1) + dr
  END DO

  ! Area at surface and bottom
  innerArea = 0.0
  DO R = 1,nr,1
    horArea(R) = PI  * cr(R)**2 - innerArea
    innerArea = innerArea + horArea(R)
    verArea(R) = 2 * PI * cr(R) * dz
  END DO

  DO Z = 1,nz,1
    DO R = 1,nr,1
      cellMass(Z,R) = dSlurry * dz * horArea(R)
    END DO
  END DO

  ! Heat transfer resistance terms R' (K-m2/W)
  Rbottom = dz / 2. / kConv + Rfloor + Rsoil 
  Rewall = dr / 2. / kConv + Rwall + Rair
  Rtop = dz / 2. / kConv + Rair 

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Start simulation
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Start main day loop
  DOY = startingDOY - 1
  YR = 1
  DO DOS = 1,nDays,1

    DOY = DOY + 1
    IF (DOY == 366) THEN
      DOY = 1
      YR = YR + 1
    END IF

    !! Start hour loop
    !sumTempSlurry = 0
    !sumQout = 0
    !sumHH = 0
    !sumHHadj = 0

    ! Hour loop
    DO TS = 1,nt,1
   
      ! Calculate heat transfer rates, all in watts (J/s)
      ! Set most heat transfer terms to 0 to start
      DO Z = 1,nz,1
        DO R = 1,nr,1
          Qrad(Z,R) = 0.0
          Qslur2air(Z,R) = 0.0
          Qslur2floor(Z,R) = 0.0
          Qslur2wall(Z,R) = 0.0
          Qconv(Z,R) = 0.0
          ! J/s         J/s-m3  *        t        /   kg/m3 * kg/t
          Qgen(Z,R) = - heatGen * cellMass(Z,R) / dSlurry * 1000.
        END DO
      END DO

      ! Radiation, top, and floor
      ! Radiation fixed for day (W = J/s) so could be outside of hourly loop
      DO R = 1,nr,1
        Qrad(nz,R) = - horArea(R) * absorp * solRad(DOY)
        Qslur2air(nz,R) = horArea(R) * (temp(nz,R) - tempAir(DOY)) / Rtop
        Qslur2floor(1,R) = horArea(R) * (temp(1,R) - tempFloor(DOY)) / Rbottom
      END DO

      ! Wall
      DO Z = 1,nz,1
        Qslur2wall(Z,nr) = verArea(R) * (temp(Z,nr) - tempWall(DOY)) / Rewall
      END DO

      ! Convection in slurry ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Remember positive is *loss*

      ! First inner cells with 4 neighbors
      DO Z = 2,nz-1,1
        DO R = 2,nr-1,1
          Qconv(Z,R) = horArea(R) * kConv * (temp(Z,R) - temp(Z-1,R)) / dz  + &  ! Bottom
                     & horArea(R) * kConv * (temp(Z,R) - temp(Z+1,R)) / dz  + &  ! Top
                     & verArea(R-1) * kConv * (temp(Z,R) - temp(Z,R-1)) / dr  + &  ! Inside
                     & verArea(R) * kConv * (temp(Z,R) - temp(Z,R+1)) / dr       ! Outside
        END DO
      END DO

      ! Next center and wall cells
      ! Not heat transfer inside
      DO Z = 2,nz-1,1
        ! Center
        R = 1
        Qconv(Z,R) = horArea(R) * kConv * (temp(Z,R) - temp(Z-1,R)) / dz  + &   ! Bottom
                   & horArea(R) * kConv * (temp(Z,R) - temp(Z+1,R)) / dz  + &   ! Top
                   & verArea(R) * kConv * (temp(Z,R) - temp(Z,R+1)) / dr        ! Outside
        
        ! Wall
        R = nr
        Qconv(Z,R) = horArea(R) * kConv * (temp(Z,R) - temp(Z-1,R)) / dz  + &   ! Bottom
                    & horArea(R) * kConv * (temp(Z,R) - temp(Z+1,R)) / dz  + &   ! Top
                    & verArea(R-1) * kConv * (temp(Z,R) - temp(Z,R-1)) / dr        ! Inside
      END DO

      ! Next, top and bottom cells
      DO R = 2,nr-1,1
        ! Top
        Z = nz
        Qconv(Z,R) = horArea(R) * kConv * (temp(Z,R) - temp(Z-1,R)) / dz  + &   ! Bottom
                    & verArea(R-1) * kConv * (temp(Z,R) - temp(Z,R-1)) / dr  + &   ! Inside
                    & verArea(R) * kConv * (temp(Z,R) - temp(Z,R+1)) / dr        ! Outside

        ! Bottom
        Z = 1
        Qconv(Z,R) = horArea(R) * kConv * (temp(Z,R) - temp(Z+1,R)) / dz  + &   ! Top
                   & verArea(R-1) * kConv * (temp(Z,R) - temp(Z,R-1)) / dr  + &   ! Inside
                   & verArea(R) * kConv * (temp(Z,R) - temp(Z,R+1)) / dr        ! Outside
      END DO

      ! A corner next
      Z = 1
      R = 1
      Qconv(Z,R)  = horArea(R) * kConv * (temp(Z,R) - temp(Z+1,R)) / dz  + &    ! Above
                  & verArea(R) * kConv * (temp(Z,R) - temp(Z,R+1)) / dr         ! Outside

      ! B corner
      Z = 1
      R = nr
      Qconv(Z,R)  = horArea(R) * kConv * (temp(Z,R) - temp(Z+1,R)) / dz  + &    ! Above
                   & verArea(R-1) * kConv * (temp(Z,R) - temp(Z,R-1)) / dr         ! Inside

      ! C corner
      Z = nz
      R = nr
      Qconv(Z,R)  = horArea(R) * kConv * (temp(Z,R) - temp(Z-1,R)) / dz  + &   ! Below
                   & verArea(R-1) * kConv * (temp(Z,R) - temp(Z,R-1)) / dr         ! Inside

      ! D corner
      Z = nz
      R = 1
      Qconv(Z,R)  = horArea(Z) * kConv * (temp(Z,R) - temp(Z-1,R)) / dz  + &   ! Below
                   & verArea(R) * kConv * (temp(Z,R) - temp(Z,R+1)) / dr        ! Outside

      ! Total heat transfer and temperature change ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO Z = 1,nz,1
        DO R = 1,nr,1
          Qtot(Z,R) = Qrad(Z,R) + Qslur2air(Z,R) + Qslur2floor(Z,R) + Qslur2wall(Z,R) + &
                    & Qconv(Z,R) + Qgen(Z,R)

          dTemp = - Qtot(Z,R) / (cpLiquid * cellMass(Z,R)) * 3600. * dt
          temp(Z,R) = temp(Z,R) + dTemp
        END DO
      END DO

    END DO

    ! Export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    DO Z = 1,nz,1
      DO R = 1,nr,1
      WRITE(10,"(I4,',',I3,',',I4,',',I3,',',I3,12(',',F8.2))") DOS, DOY, YR, Z, R, &
               & Z * dz - dz / 2, R * dr - dr / 2, tempAir(DOY), tempFloor(DOY), & 
               & temp(Z,R), Qrad(Z,R), Qslur2air(Z,R), Qslur2floor(Z,R), Qslur2wall(Z,R), &
               & Qconv(Z,R), Qgen(Z,R), Qtot(Z,R)
      END DO
    END DO

  END DO

  ! Export dimension info for checking
  DO R = 1,nr,1
    WRITE(11,"(I4,',',5(',',F9.3))") R, dr, cr(R), dz, horArea(R), verArea(R)
  END DO

  DO Z = 1,nz,1
    DO R = 1,nr,1
      WRITE(12,"(2(I4,','),F15.3)") Z, R, cellMass(Z,R)
    END DO
  END DO


  WRITE(20,'(A)') 'Done!'
  WRITE(*,'(A)') 'Done!'

  ! Timer
  CALL SYSTEM_CLOCK(ttend)
  WRITE(20,'(A,1X,F8.4,1X,A)') 'Run time: ', real(ttend - ttbeginning) / real(ttrate), ' seconds'

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Close files
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CLOSE(2)
  CLOSE(10)
  CLOSE(11)
  CLOSE(20)

END PROGRAM stm
