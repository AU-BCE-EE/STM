PROGRAM stm

  ! Simple temperature model for stored slurry 
  ! By Sasha D. Hafner
  ! See https://github.com/sashahafner/STM for latest version and more details

  IMPLICIT NONE
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Declaration statements 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Days, and other integers
  INTEGER :: HR          ! Hour of day (1-24)
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
  INTEGER :: nz, nr 

  ! Simulation ID
  CHARACTER (LEN=10) :: ID ! ID code of simulation
  !!CHARACTER (LEN=1) :: ventType ! Type of ventilation

  ! File names
  CHARACTER (LEN=30) :: userParFile, parFile, weatherFile, levelFile

  ! Version string
  CHARACTER (LEN=45) :: verString = 'Starting STM version 1.0 (9 March 2023)'

  ! Command line arguments, length
  INTEGER :: numArgs

  ! Temperatures, all in degrees C
  REAL, DIMENSION(366) :: tempAir, tempFloor, tempWall ! Air, floor (bottom of store or pit), and wall (side of store or pit)
  REAL :: tempAirSum, tempAirAve ! Average air temperature
  REAL :: dTemp          ! Change in temperature during time step (deg. C) 
  REAL :: maxAnnTemp     ! Maximum daily average air temperature over the year
  REAL :: minAnnTemp     ! Minimum daily average air temperature over the year
  REAL :: tempInitial    ! Initial slurry temperature
  REAL :: tempSlurry     ! Hourly slurry temperature 
  REAL :: tempSS         ! Steady-state slurry temperature used to deal with numerical instability
  REAL :: tempIn         ! Temperature of slurry when added to store/pit/tank/lagoon
  CHARACTER (LEN=5) :: tempInChar! Temperature of slurry when added to store/pit/tank/lagoon as character for flexible reading in
  REAL :: trigPartTemp   ! Sine part of temperature expression (intermediate in calculation)
  REAL :: residVol      ! Mass of slurry left behind when emptying

  ! Sums and averages
  REAL :: sumTempSlurry  ! Sum of hourly slurry temperatures for calculating daily mean
  REAL :: sumMassSlurry  ! Sum of 
  REAL :: sumSlurryProd  ! Sum of 
  REAL :: aveMassSlurry  ! Average
  REAL :: aveSlurryProd  ! Average
  REAL :: retentionTime

  ! Geometry of storage structure
  REAL :: slurryDepth    ! Depth of slurry in store/pit (m)
  REAL :: storeDepth     ! Total depth of store/pit (m)
  REAL :: buriedDepth    ! Buried depth (m)
  REAL :: wallDepth      ! Depth at which horizontal heat transfer to soil is evaluated (m) 
  REAL :: storeDia, width  ! Length and width of store/pit (m)
  REAL :: areaFloor      ! Storage floor area in contact with soil (m2)
  REAL :: areaDwall      ! Storage wall area (D for down) in contact with soil (buried) (m2)
  REAL :: areaUwall      ! Storage wall area (U for upper) in contact with air (above ground) (outside) and slurry (inside) (m2)
  REAL :: areaAir        ! Slurry upper surface area (m2)
  !INTEGER :: nStores   ! Number of stores or pits

  ! Solar
  REAL :: areaSol        ! Area intercepting solar radiation (m2) 
  REAL :: absorp         ! Slurry or cover effective absorptivity (dimensionless)
  REAL :: maxAnnRad      ! Maximum daily average radiation over the year
  REAL :: minAnnRad      ! Minimum daily average radiation over the year
  REAL, DIMENSION(366) :: solRad ! Average solar radiation rate (W/m2)
  REAL :: trigPartRad    ! Sine part of radiation expression (intermediate in calculation)
  
  ! Other slurry variables
  REAL :: slurryMassTot, slurryMassTotInit, massFrozen = 0   ! Slurry mass (Mg = 1000 kg = metric tonnes)
  REAL :: slurryVol      ! Initial slurry volume (m3) NTS not consistent name
  REAL :: slurryProd     ! Slurry production rate (= inflow = outflow) (Mg/d)

  ! Level variables
  REAL, DIMENSION(365) :: level, rLevelAve
  REAL :: levelPrev

  ! Heat transfer coefficients and related variables
  REAL :: Rair, Rwall, Rfloor, Rslur, Rsoil ! User-entered resistance terms K-m2/W
  REAL :: Ruwall, Rdwall, Rbottom, Rtop ! Calculated resistance K-m2/W 
  REAL :: cpSlurry            ! Heat capacity of slurry J/kg-K
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
  REAL :: Qrad           ! "To" sun
  REAL :: Qslur2air      ! To air
  REAL :: Qslur2wall, Qslur2dwall, Qslur2uwall   ! Out through wall (to air or soil)
  REAL :: Qslur2floor    ! Out through floor to soil
  REAL :: Qfeed          ! Loss due to feeding
  REAL :: Qgen           ! Heat generation
  REAL :: Qout           ! Total
  REAL :: QoutPart       ! Total after some use for melting/freezing
  REAL :: sumQout        ! Sum of total for average

  ! 2D stuff
  REAL, DIMENSION(100) :: dz, dr         ! m
  REAL, DIMENSION(100, 100) :: cellVol        !

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
  INTEGER, DIMENSION(8) :: dt 
  CHARACTER (LEN = 10) :: date

  ! Constant
  CONSTANT :: PI = 3.14159
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
  ELSE IF (numArgs .EQ. 5) THEN
    CALL GET_COMMAND_ARGUMENT(1, ID)
    CALL GET_COMMAND_ARGUMENT(2, parFile)
    CALL GET_COMMAND_ARGUMENT(3, userParFile)
    CALL GET_COMMAND_ARGUMENT(4, weatherFile)
    CALL GET_COMMAND_ARGUMENT(5, levelFile)
    calcWeather = .FALSE.
    fixedFill = .FALSE.
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
  IF (.NOT. fixedFill) THEN
    OPEN (UNIT=4, FILE=levelFile, STATUS='OLD')
  END IF

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Output and log files
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Output files, name based on ID
  OPEN (UNIT=10,FILE=(''//TRIM(ID)//'_temp.csv'), STATUS='UNKNOWN')
  OPEN (UNIT=11,FILE=(''//TRIM(ID)//'_rates.csv'), STATUS='UNKNOWN')
  OPEN (UNIT=12,FILE=(''//TRIM(ID)//'_weather.csv'), STATUS='UNKNOWN')
  !OPEN (UNIT=13,FILE=(''//TRIM(ID)//'_summary.txt'), STATUS='UNKNOWN')

  ! Log file, name based on ID
  OPEN (UNIT=20,FILE=(''//TRIM(ID)//'_log.txt'), STATUS='UNKNOWN')

  ! Start timer
  CALL SYSTEM_CLOCK(ttbeginning, ttrate)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Start log file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  WRITE(*,'(A)') verString 
  WRITE(20,'(A)') verString 
  CALL DATE_AND_TIME(DATE = date, VALUES = dt)
  WRITE(20,'(A, I4, 5(A, I2.2))') 'Date and time: ', dt(1), '/', dt(2), '/', dt(3), ' ', dt(5), ':', dt(6), ':', dt(7)
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
  READ(1,*) storeDepth
  READ(1,*) buriedDepth
  READ(1,*) storeDia
  READ(1,*) width
  READ(1,*) areaAir
  READ(1,*) areaSol
  IF (fixedFill) THEN
    READ(1,*) slurryVol
  ELSE
    READ(1,*)
    slurryVol = 0.0
  END IF
  READ(1,*) tempInitial
  READ(1,*) tempInSetting
  READ(1,*) tempInChar
  IF (fixedFill) THEN
    READ(1,*) slurryProd
    READ(1,*) residVol
    READ(1,*) emptyDOY1
    READ(1,*) emptyDOY2 
  END IF

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
  READ(2,*) Rair, Rwall, Rfloor, Rslur, Rsoil
  READ(2,*) 
  READ(2,*) 
  READ(2,*) 
  READ(2,*) absorp, soilConstDepth, soilOffset, heatGen

  ! Output file header
  WRITE(10,"(A)") 'Day of sim.,Day of year,Year,Slurry mass,Frozen mass,Slurry depth,Air T,Wall T,Floor T,In T,Slurry T'
  WRITE(10,"(A)") ',,,(Mg = 1000 kg),(Mg = 1000 kg),(m),(deg. C),(deg. C),(deg. C),(deg. C),(deg. C)'
  WRITE(10,"(A)") 'day,doy,year,slurry_mass,frozen_mass,slurry_depth,air_temp,wall_temp,floor_temp,in_temp,slurry_temp'
 

  WRITE(11,"(A)") 'Day of sim.,Day of year,Year,Radiation,Generation,Air,Floor,Lower wall,Upper wall,Feed,Total,Total step,&
    &          Total step adjusted,Steady state temp,Steady state used'
  WRITE(11,"(A)") ',,,(W),(W),(W),(W),(W),(W),(W),(W),(J),(J),,'
  WRITE(11,"(A)") 'day,doy,year,rad,gen,air,floor,lower_wall,upper_wall,feed,total,total_step,total_adjusted,&
    &          steady_state_used,steady_state_temp'

  WRITE(12,"(A)") 'Day of sim.,Day of year,Year,Air T,Radiation'
  WRITE(12,"(A)") ',,,(deg. C),(W/m2)'
  WRITE(12,"(A)") 'day,doy,year,air_temp,rad'

  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Initial calculations
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Sort out tempreature of added slurry
  IF (tempInsetting .EQ. 'Con' .OR. tempInSetting .EQ. 'con') THEN
    constantTempIn = .TRUE.
    READ(tempInChar, *) tempIn
  ELSE IF (tempInSetting .EQ. 'Non' .OR. tempInSetting .EQ. 'non' .OR. tempInsetting .EQ. 'Sam' .OR. tempInSetting .EQ. 'sam') THEN
    constantTempIn = .FALSE.
    airTempIn = .FALSE.
    tempIn = -99
  ELSE IF (tempInsetting .EQ. 'Air' .OR. tempInSetting .EQ. 'air') THEN
    constantTempIn = .FALSE.
    airTempIn = .TRUE.
    tempIn = -99
  ELSE
    WRITE(20,*) "Error: Added slurry temperature description not recognized. Must be Constant or None (or Same). Stopping."
    WRITE(*,*) "Error: Added slurry temperature description not recognized. Must be Constant or None (or Same). Stopping."
    STOP
  END IF

  ! Calculate some geometry-related variables
  wallDepth = 0.5 * buriedDepth             ! m
  IF (width .EQ. 0.) THEN
    ! Circular
    areaFloor = PI * (storeDia / 2.)**2       ! m2
  ELSE 
    ! Rectangular
    STOP
  END IF
  slurryMassTot = slurryVol * dSlurry / 1000. ! Slurry mass is in metric tonnes = Mg = 1000 kg
  slurryMassTotInit = slurryMassTot

  ! Heat transfer resistance terms R' (K-m2/W)
  Rbottom = Rslur + Rfloor + Rsoil
  Rdwall = Rslur + Rwall + Rsoil 
  Ruwall = Rslur + Rwall + Rair
  Rtop = Rslur + Rair 

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
  wallAvePeriod = MIN(wallDepth/soilConstDepth, 1.)*365
  floorAvePeriod = MIN(buriedDepth/soilConstDepth, 1.)*365

  ! Avoid very short averaging periods that are not plausible (because floor is actually covered with tank, so 1 d impossible)
  IF (floorAvePeriod .LT. 5.) THEN
    floorAvePeriod = 5.
  END IF

  IF (wallAvePeriod .LT. 2.) THEN
    wallAvePeriod = 2.
  END IF

  ! Calculate moving average for first day of year
  ! Wall first
  IF (wallAvePeriod .GE. 365) THEN
    tempWall(:) = tempAirAve
  ELSE IF (wallAvePeriod > 1) THEN
    ! First need to calculate value for DOY = 1
    tempWall(1) = tempAir(1) / wallAvePeriod
    DO DOY = 365 - wallAvePeriod + 2,365,1
      tempWall(1) = tempWall(1) + tempAir(DOY)/wallAvePeriod
    END DO
    IF (tempWall(1) .LT. 0.) THEN
      tempWall(1) = tempWall(1) / soilFreezeDiv
    END IF

    DO DOY = 2,365,1
      IF (DOY > wallAvePeriod) THEN
        tempWall(DOY) = tempWall(DOY - 1) - tempAir(DOY - wallAvePeriod)/wallAvePeriod + tempAir(DOY)/wallAvePeriod
      ELSE 
        tempWall(DOY) = tempWall(DOY - 1) - tempAir(365 + DOY - wallAvePeriod)/wallAvePeriod + tempAir(DOY)/wallAvePeriod
      END IF 
      IF (tempWall(DOY) .LT. 0.) THEN
        tempWall(DOY) = tempWall(DOY) / soilFreezeDiv
      END IF
    END DO

  ELSE
    tempWall(:) = tempAir(:)
    DO DOY = 1,365,1
      IF (tempWall(DOY) .LT. 0.) THEN
        tempWall(DOY) = tempWall(DOY) / soilFreezeDiv
      END IF
    END DO
  END IF

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

  ! Reading in slurry level
  ! Set all to NA value
  level = -99.
  IF (.NOT. fixedFill) THEN
    READ(4,*,IOSTAT=fileStat) ! Header
    READ(4,*) DOY, level(1)
    levelPrev = level(1)
    IF (DOY .NE. 1.) THEN
      WRITE(20,*) "Error: First day of year *must* be 1 in level file! Stopping."
      WRITE(*,*) "Error: First day of year *must* be 1 in level file! Stopping."
      STOP
    END IF
    DOYprev = 1
    level(365) = level(1)
    READ(4,*,IOSTAT=fileStat) DOY, level(DOY)
    DO WHILE (.NOT. IS_IOSTAT_END(fileStat))
      IF (DOY .NE. DOYprev) THEN
        rLevelAve(DOYprev) = (level(DOY) - levelPrev) / (DOY - DOYprev)
        DOYprev = DOY
        levelPrev = level(DOY)
      END IF
      READ(4,*,IOSTAT=fileStat) DOY, level(DOY)
    END DO
    IF (DOY .NE. 365) THEN
      rLevelAve(DOYPrev) = (level(365) - levelPrev) / (365 - DOYprev)
    ELSE
      rlevelAve(DOYPrev) = 0
    END IF

    ! Fill in missing levels, linear interpolation
    DO DOY = 2,365,1
      IF (level(DOY) .LT. 0) THEN
        level(DOY) = level(DOY - 1) + rLevelAve(DOY - 1) * 1.
        rLevelAve(DOY) = rLevelAve(DOY - 1)
      END IF
    END DO

  END IF


  ! Set initial slurry temperature and specific heat
  tempSlurry = tempInitial
  cpSlurry = cpLiquid

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Cells
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! R = 1 is centermost ring
  ! Z = 1 is bottom layer

  dz = storeDepth / nz
  dr = storeDepth / nr

  ! Area at surface and bottom
  DO R = 1,nr,1
    horArea(R) = PI  * (2 * (R + dr) + dr**2)
  END DO

  ! Area under wall
  DO Z = 1,nz,1
    wallArea(Z,nr) = 2. * PI * storeDia * dz
    DO R = 1,nr - 1, 1
      wallArea(Z,R) = 0.
    END DO
  END DO

  ! Circular
  areaWall = slurryDepth * PI * storeDia 

  !! Mass of slurry in each cell
  !ncells = nr * nz

  DO Z = 1,nz,1
    DO R = 1,nr,1
      cellMass(Z,R) = dSlurry * dz * horArea(R)
    END DO
  END DO

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

    ! Radiation fixed for day (W = J/s) so outside of hourly loop
    ! Radiation is 0 except for surface
    DO R = 1,nr,1
      DO Z = 1,nz - 1,1
       Qrad(Z,R) = 0.
      END DO
      Qrad(nz,R) = - horArea(R) * absorp * solRad(DOY)
    END DO

    ! Start hour loop
    sumTempSlurry = 0
    sumQout = 0
    sumHH = 0
    sumHHadj = 0

    ! Hour loop
    DO HR = 1,24,1
   
      ! Calculate heat transfer rates, all in watts (J/s)
      ! Set all heat transfer terms to 0 to start
      DO Z = 1,nz,1
        DO R = 1,nr,1
          Qfeed(Z,R) = 0.0
          Qslur2air(Z,R) = 0.0
          Qslur2floor(Z,R) = 0.0
          Qslur2dwall(Z,R) = 0.0
          Qslur2uwall(Z,R) = 0.0
          ! J/s         J/s-m3  *        t        /   kg/m3 * kg/t
          Qgen(Z,R) = - heatGen * slurryMass(Z,R) / dSlurry * 1000.
        END DO
      END DO

      ! Floor and air
      DO R = 1,nr,1
        Qslur2floor(1,R) = (tempSlurry(1,R) - tempFloor(DOY)) / Rbottom * horArea(1,R)
        Qslur2air(nz,R) = (tempSlurry(1,R) - tempAir(DOY)) / Rtop * horArea(1,R)
      END DO

      ! Wall
      DO Z = 1,nz,1
        Qslur2wall(Z,nr) = (tempSlurry(Z,nr) - tempWall(DOY)) / Rdwall * wallArea(Z,nr)
      END DO

      ! Convection in slurry
      ! Remember positive is *loss*
      ! Top
      Z = nz
      DO R = 1,nr-1,1
        Qslur = thermK * (tempSlurry(Z,R) - tempSlurry(Z-1,R)) * 
      END DO

      ! Total
      DO Z = 1,nz,1
        DO R = 1,nr,1
          Qout(Z,R) = Qfeed(Z,R) + Qrad(Z,R) + Qslur2air(Z,R) + Qslur2wall(Z,R) + Qslur2floor(Z,R) + Qgen(Z,R)
          ! HH in J
          !J =  W   *  s/h  * h
          HH = Qout * 3600. * 1.
        END DO
      END DO

      ! Melt any frozen slurry
      HHadj = HH + 1000. * massFrozen * hfSlurry
      massFrozen = 0.0

      ! Potential temperature change (omitting latent energy and assuming liquid slurry for now)
      dTemp = - HHadj / (1000. * cpSlurry * slurryMassTot)

      ! Freeze and thaw
      IF (tempSlurry + dTemp .LT. tempFreeze) THEN
        ! Use HH to get to 0 C
        HHadj = HHadj + (tempFreeze - tempSlurry) * 1000. * cpSlurry * slurryMassTot
        tempSlurry = tempFreeze

        IF (HHadj .GT. 0.0) THEN
          ! Still some cooling available for freezing
          massFrozen = HHadj / hfSlurry / 1000.
          IF (massFrozen .GE. slurryMassTot) THEN
            ! More than enough to freeze all slurry
            massFrozen = slurryMassTot
            HHadj = HHadj - 1000. * massFrozen * hfSlurry
          ELSE
            ! Only some freezes and temperature stays at 0C
            ! massFrozen from above applies
            HHadj = 0.0
          END IF
        END IF
      END IF

      ! Weighted cp
      cpSlurry = massFrozen / slurryMassTot * cpFrozen + (1.0 - massFrozen / slurryMassTot) * cpLiquid

      ! Recalculate dT, now actual temperature change
      dTemp = - HHadj / (1000. * cpSlurry * slurryMassTot)
      tempSlurry = tempSlurry + dTemp
      
      ! Steady-state temperature
      tempSS = (tempAir(DOY)/Rtop*areaAir + tempFloor(DOY)/Rbottom*areaFloor + tempWall(DOY)/Rdwall*areaDwall + &
        & tempAir(DOY)/Ruwall*areaUwall + (1000. * slurryProd / 86400. * cpLiquid * tempIn) - Qrad - Qgen) / &
        & (areaAir/Rtop + areaFloor/Rbottom + areaDwall/Rdwall + areaUwall/Ruwall + (1000. * slurryProd / 86400. * cpLiquid))

      ! Limit change in temperature to change to SS temp
      IF (dTemp > 0. .AND. tempSlurry > tempSS) THEN
        tempSlurry = tempSS
        useSS = .TRUE.
      ELSEIF (dTemp < 0. .AND. tempSlurry < tempSS) THEN
        tempSlurry = tempSS
        useSS = .TRUE.
      END IF
      
      sumTempSlurry = sumTempSlurry + tempSlurry
      sumQout = sumQout + Qout
      sumHH = sumHH + HH
      sumHHadj = sumHHadj + HHadj

    END DO

    sumMassSlurry = sumMassSlurry + slurryMassTot
    sumSlurryProd = sumSlurryProd + slurryProd

    WRITE(10,"(I4,',',I3,',',I4,8(',',F8.2))") DOS, DOY, YR, slurryMassTot, massFrozen, slurryDepth, tempAir(DOY), & 
        & tempWall(DOY), tempFloor(DOY), tempIn, sumTempSlurry/24.

    WRITE(11,"(I4,',',I3,',',I4,6(',',F11.3),4(',',F15.0),',',1F5.2,',',L5)") DOS, DOY, YR, Qrad, Qgen, Qslur2air, &
        & Qslur2floor, Qslur2dwall, Qslur2uwall, Qfeed, sumQout/24., sumHH/24., sumHHadj/24., tempSS, useSS

    WRITE(12,"(I4,',',I3,',',I4,2(',',F15.2))") DOS, DOY, YR, tempAir(DOY), solRad(DOY)
    
  END DO

  aveMassSlurry = sumMassSlurry / DBLE(nDays)
  aveSlurryProd = sumSlurryProd /  DBLE(nDays)
  retentionTime = aveMassSlurry / aveSlurryProd
  !WRITE(13, *) 'Average slurry mass (t): ', aveMassSlurry
  !WRITE(13, *) 'Average addition rate (t/d): ', aveSlurryProd
  !WRITE(13, *) 'Average retention time (d): ', retentionTime

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
