PROGRAM stm

  ! Simple temperature model for slurry in stores or pits in a barn
  ! Date           Who                         Description
  ! 28 FEB 2015   S. Hafner                    Original code (stole some from tmad.f90)
  ! 01 MAR 2015   S. Hafner                    Added calculation of substrate temperatures and heat transfer coefficients
  !                                            First realistic version
  ! 31 MAR 2015   S. Hafner                    Added option for mechanical ventilation with heating, changed calculation of air
  !                                            temperature and substrate temperatures
  ! 08 MAY 2015   S. Hafner                    Changed name of transport_parameters.txt to transfer_parameters.txt 
  ! 26 May 2020   S. Hafner                    Create STM repo, will use Git for tracking development

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

  ! Simulation ID
  CHARACTER (LEN=4) :: ID ! ID code of simulation
  !!CHARACTER (LEN=1) :: ventType ! Type of ventilation

  ! File names
  CHARACTER (LEN=30) :: userParFile, parFile, weatherFile, levelFile

  ! Command line arguments, length
  INTEGER :: numArgs

  ! Temperatures, all in degrees C
  REAL, DIMENSION(366) :: tempAir, tempFloor, tempWall ! Air, floor (bottom of store or pit), and wall (side of store or pit)
  REAL :: dTemp          ! Change in temperature during time step (deg. C) 
  REAL :: maxAnnTemp     ! Maximum daily average air temperature over the year
  REAL :: minAnnTemp     ! Minimum daily average air temperature over the year
  REAL :: tempInitial    ! Initial slurry temperature
  REAL :: tempSlurry     ! Hourly slurry temperature 
  REAL :: tempSS         ! Steady-state slurry temperature used to deal with numerical instability
  REAL :: sumTempSlurry  ! Sum of hourly slurry temperatures for calculating daily mean
  REAL :: tempIn         ! Temperature of slurry when added to store/pit/tank/lagoon
  REAL :: trigPartTemp   ! Sine part of temperature expression
  REAL :: residMass      ! Mass of slurry left behind when emptying

  ! Geometry of storage structure
  REAL :: slurryDepth    ! Depth of slurry in store/pit (m)
  REAL :: storeDepth     ! Total depth of store/pit (m)
  REAL :: buriedDepth    ! Buried depth (m)
  REAL :: wallDepth      ! Depth at which horizontal heat transfer to soil is evaluated (m) 
  REAL :: length, width  ! Length and width of store/pit (m)
  REAL :: areaFloor      ! Storage floor area in contact with soil (m2)
  REAL :: areaDwall      ! Storage wall area (D for down) in contact with soil (buried) (m2)
  REAL :: areaUwall      ! Storage wall area (U for upper) in contact with air (above ground) (outside) and slurry (inside) (m2)
  REAL :: areaAir        ! Slurry upper surface area (m2)
  INTEGER :: nStores   ! Number of stores or pits

  ! Solar
  REAL :: areaSol        ! Area intercepting solar radiation (m2) 
  REAL :: absorp         ! Slurry or cover effective absorptivity (dimensionless)
  REAL :: maxAnnRad      ! Maximum daily average radiation over the year
  REAL :: minAnnRad      ! Minimum daily average radiation over the year
  REAL, DIMENSION(366) :: solRad ! Average solar radiation rate (W/m2)
  REAL :: trigPartRad    ! Sine part of radiation expression
  
  ! Other slurry variables
  REAL :: massSlurry, massFrozen = 0   ! Slurry mass (Mg = 1000 kg = metric tonnes)
  REAL :: slurryVol      ! Initial slurry volume (m3) NTS not consistent name
  REAL :: slurryProd     ! Slurry production rate (= inflow = outflow) (Mg/d)

  ! Level variables
  REAL, DIMENSION(366) :: level, rLevelAve
  REAL :: levelPrev

  ! Heat transfer coefficients and related variables
  REAL :: Rair, Rconc, Rslur, Rsoil ! Entered resistance terms K-m2/W
  REAL :: Ruwall, Rdwall, Rfloor, Rtop ! Calculated resistance K-m2/W 
  REAL :: cpSlurry       ! Heat capacity of slurry J/kg-K
  REAL :: hfSlurry       ! Latent heat of fusion of slurry J/kg
  REAL :: dSlurry        ! Density of slurry kg/m3
  REAL :: soilDamp       ! Soil damping depth, where averaging period reaches 1 full yr, in m

  ! Heat flow variables in W out of slurry
  REAL :: Qrad           ! "To" sun
  REAL :: Qslur2air      ! To air
  REAL :: Qslur2wall, Qslur2dwall, Qslur2uwall   ! Out through wall (to air or soil)
  REAL :: Qslur2floor    ! Out through floor to soil
  REAL :: Qfeed          ! Loss due to feeding
  REAL :: Qout           ! Total
  REAL :: QoutPart       ! Total after some use for melting/freezing
  REAL :: sumQout        ! Sum of total for average
  REAL :: sumHH, sumHHadj ! Sum for average

  ! Heat flow total
  REAL :: HH             ! Total heat flow out of slurry in a time step
  REAL :: HHadj          ! Total adjusted for melting slurry in J

  LOGICAL :: calcWeather ! .TRUE. when weather inputs are calculated (otherwise read from file)
  LOGICAL :: fixedFill   ! .TRUE. when slurry is added at a fixed rate, specified in user par file
  LOGICAL :: useSS       ! .TRUE. when steady-state temperature was used at some point within a day
  LOGICAL :: constantTempIn  ! .TRUE. when temperature of added slurry is constant and added slurry brings heat energy in

  CHARACTER (LEN = 2) tempInSetting

  ! Other parameters
  REAL, PARAMETER :: PI = 3.1415927
  REAL, PARAMETER :: soilFreezeDiv = 1.0  ! Fudge factor for soil freezing
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
    WRITE(*,*) 'No file names given so 2 default parameter files will be used, with calculated weather and ID 0001.'
    parFile = 'pars.txt'
    userParFile = 'user_pars.txt'
    ID = '0001'
    calcWeather = .TRUE.
    fixedFill = .TRUE.
  END IF


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Open files
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Input files
  OPEN (UNIT=1, FILE=userParFile, STATUS='OLD')
  OPEN (UNIT=2, FILE=parFile, STATUS='OLD')
  IF (.NOT. calcWeather) THEN
    OPEN (UNIT=3, FILE=weatherFile, STATUS='UNKNOWN')
  END IF
  IF (.NOT. fixedFill) THEN
    OPEN (UNIT=4, FILE=levelFile, STATUS='UNKNOWN')
  END IF

  ! Output files, name based on ID
  OPEN (UNIT=10,FILE=(''//ID//'_temp.txt'),STATUS='UNKNOWN')
  OPEN (UNIT=11,FILE=(''//ID//'_rates.txt'),STATUS='UNKNOWN')
  OPEN (UNIT=12,FILE=(''//ID//'_weather.txt'),STATUS='UNKNOWN')
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Read parameters
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! User settings
  READ(1,*)
  READ(1,*) nDays
  READ(1,*) startingDOY
  READ(1,*) nStores
  READ(1,*) storeDepth
  READ(1,*) buriedDepth
  READ(1,*) length
  READ(1,*) width
  READ(1,*) areaAir
  READ(1,*) areaSol
  READ(1,*) slurryVol
  READ(1,*) tempInitial
  READ(1,*) tempInSetting
  READ(1,*) tempIn
  READ(1,*) slurryProd
  READ(1,*) residMass
  READ(1,*) emptyDOY1
  READ(1,*) emptyDOY2 

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
  READ(2,*) dSlurry, cpSlurry, hfSlurry
  READ(2,*) 
  READ(2,*) 
  READ(2,*) Rair, Rconc, Rslur, Rsoil
  READ(2,*) 
  READ(2,*) 
  READ(2,*) 
  READ(2,*) absorp, soilDamp

  ! Output file header
  WRITE(10,*) 'Day of  Day of    Year   Slurry  Frozen  Slurry   Air    Wall   Floor    Slurry'
  WRITE(10,*) 'sim.     year             mass    mass   depth     T      T       T        T'
 

  WRITE(11,*) 'Day of  Day of              -------------Heat flow rates out in W----------------------------------------------'
  WRITE(11,*) 'sim.     year     Year    Radiation    Air       Floor   Lower wall  Upper wall   Feed      Total    Total ave. &  
    &      HH            HHave          HHadj         HHadjave   SST      SS'

  WRITE(12,*) 'Day of  Day of Year Air   Radiation'
  WRITE(12,*) 'sim.     year        T'
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Initial calculations
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Sort out tempreature of added slurry
  IF (tempInsetting .EQ. 'Co') THEN
    constantTempIn = .TRUE.
  ELSE IF (tempInSetting .EQ. 'No') THEN
    constantTempIn = .FALSE.
  ELSE
    WRITE(*,*) "Error: Addes slurry temperature description not recognized. Must be Constant or None. Stopping."
    STOP
  END IF

  ! Calculate some geometry-related variables
  wallDepth = 0.5 * buriedDepth             ! m
  IF (width .EQ. 0.) THEN
    ! Circular
    areaFloor = PI * (length / 2.)**2   ! m2
  ELSE 
    ! Rectangular
    areaFloor = width*length*nStores      ! m2
  END IF
  massSlurry = slurryVol * dSlurry / 1000 ! Slurry mass is in metric tonnes = Mg = 1000 kg

  ! Heat transfer resistance terms R' (K-m2/W)
  Rfloor = Rslur + Rconc + Rsoil
  Rdwall = Rslur + Rconc + Rsoil 
  Ruwall = Rslur + Rconc + Rair
  Rtop = Rslur + Rair 

  ! Determine tempAir, solRad, and substrate temperatures for a complete year
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
  ELSE ! Read weather data from file
    ! Skip header
    READ(3,*)
    DO WHILE (.NOT. IS_IOSTAT_END(fileStat))
      READ(3,*,IOSTAT=fileStat) DOY, tempAir(DOY), solRad(DOY)
    END DO
  END IF

  ! Substrate temperature based on moving average
  wallAvePeriod = MAX(wallDepth/soilDamp*365, 1.)
  floorAvePeriod = MAX(buriedDepth/soilDamp*365, 1.)

  ! Avoid very short averaging periods that are not plausible (because floor is actually covered with tank, so 1 d impossible)
  IF (floorAvePeriod .LT. 5.) THEN
    floorAvePeriod = 5.
  END IF
  IF (wallAvePeriod .LT. 2.) THEN
    wallAvePeriod = 2.
  END IF

  ! Calculate moving average for first day of year
  ! Wall first
  IF (wallAvePeriod > 365) THEN
    tempWall(:) = (minAnnTemp + maxAnnTemp)/2.
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
  IF (floorAvePeriod > 365) THEN
    tempFloor(:) = (minAnnTemp + maxAnnTemp)/2.
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

  ! Reading in level
  ! Set all to NA value
  level = -99.
  IF (.NOT. fixedFill) THEN
    READ(4,*,IOSTAT=fileStat) ! Header
    READ(4,*) DOY, level(1)
    levelPrev = level(1)
    IF (DOY .NE. 1.) THEN
      WRITE(*,*) "Error: First day of year *must* be 1 in level file! Stopping."
      STOP
    END IF
    DOYprev = 1
    level(365) = level(1)
    DO WHILE (.NOT. IS_IOSTAT_END(fileStat))
      READ(4,*,IOSTAT=fileStat) DOY, level(DOY)
      rLevelAve(DOYprev) = (level(DOY) - levelPrev) / (DOY - DOYprev)
      DOYprev = DOY
      levelPrev = level(DOY)
    END DO
    rLevelAve(DOY) = (level(365) - levelPrev) / (365 - DOYprev)

    ! Fill in missing levels, linear interpolation
    DO DOY = 2,365,1
      IF (level(DOY) .LT. 0) THEN
        level(DOY) = level(DOY - 1) + rLevelAve(DOY - 1) * 1.
        rLevelAve(DOY) = rLevelAve(DOY - 1)
      END IF
    END DO

  END IF


  ! Set initial slurry temperature
  tempSlurry = tempInitial

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

    ! Sort out filling rate or fixed emptying
    ! Assumes given level is for end of day
    IF (.NOT. fixedFill) THEN
      IF (DOY == 1) THEN
        levelPrev = level(365)
      ELSE 
        levelPrev = level(DOY - 1)
      END IF
      IF (level(DOY) .GT. levelPrev) THEN
        ! NTS: some inconsistency about slurry level defined at beginning or end of day
        massSlurry = levelPrev * areaFloor * dSlurry/1000.
        ! Daily slurry addition
        slurryProd = (level(DOY) - levelPrev) * areaFloor * dSlurry/1000.
      ELSE IF (level(DOY) .EQ. levelPrev) THEN
        ! No addition
        slurryProd = 0.0
        massSlurry = level(DOY) * areaFloor * dSlurry/1000.
      ELSE IF (level(DOY) .LT. levelPrev) THEN
        ! Removal, so fixed slurry mass
        ! NTS: removal at beginning of day, addition at end. . .
        massSlurry = level(DOY) * areaFloor * dSlurry/1000.
        slurryProd = 0.0
        IF (massFrozen .GT. massSlurry) THEN
          massFrozen = massSlurry
        END IF
      END IF
      levelPrev = level(DOY)
    ELSE
      ! Empty and add slurry at beginning of day
      IF (DOY == emptyDOY1 .OR. DOY == emptyDOY2) THEN
        massSlurry = residMass
        IF (massFrozen .GT. massSlurry) THEN
          massFrozen = massSlurry
        END IF
      END IF
    END IF

    ! Radiation fixed for day (W = J/s)
    Qrad = - absorp * solRad(DOY) * areaSol

    ! Start hour loop
    sumTempSlurry = 0
    sumQout = 0
    sumHH = 0
    sumHHadj = 0

    ! Set use steady state indicator to false at start of each day
    useSS = .FALSE.
    DO HR = 1,24,1

      ! Update slurry mass, distributing inflow evenly across day
      !   t           t            t/d     / h/d * h = t         
      massSlurry = massSlurry + slurryProd / 24. * 1

      ! Get depth and wall area
      slurryDepth = 1000 * massSlurry / (dSlurry*areaFloor)  ! Slurry depth in m
      IF (width .EQ. 0.) THEN
        ! Circular
        areaDwall = MIN(slurryDepth, buriedDepth) * PI * length * nStores
        areaUwall = MAX(slurryDepth - buriedDepth, 0.) * PI * length * nStores
      ELSE 
        ! Rectangular
        areaDwall = MIN(slurryDepth, buriedDepth) * 2. * (length + width) * nStores
        areaUwall = MAX(slurryDepth - buriedDepth, 0.) * 2. * (length + width) * nStores
      END IF

      ! Check slurry depth
      IF (slurryDepth .GT. 1.01 * storeDepth) THEN
        WRITE(*,*) "Slurry depth is greater than maximum depth! Stopping. Check inputs."
        STOP
      END IF
    
      ! Calculate heat transfer rates, all in watts (J/s)
      ! Qfeed is hypothetical rate pretending to make up difference relative to new mass at tempSlurry
      IF (.NOT. constantTempIn) THEN
        ! Overwrite feed temperature if there is no heat transfer in feed
        tempIn = tempSlurry
      END IF
      ! J/s               K           kg/t      t/d     / s/d    *  J/kg-K   
      Qfeed = (tempSlurry - tempIn) * 1000 * slurryProd / 86400. * cpSlurry
      ! J/s   J/s-m2-K     K               K          m2
      Qslur2air = (tempSlurry - tempAir(DOY)) / Rtop * areaAir
      Qslur2floor = (tempSlurry - tempFloor(DOY)) / Rfloor * areaFloor
      Qslur2dwall = (tempSlurry - tempWall(DOY)) / Rdwall * areaDwall
      Qslur2uwall = (tempSlurry - tempAir(DOY)) / Ruwall * areaUwall
      ! W
      Qslur2wall = Qslur2dwall + Qslur2uwall
      Qout = Qfeed + Qrad + Qslur2air + Qslur2wall + Qslur2floor
      ! HH in J
      !J =  W   *  s/h  * h
      HH = Qout * 3600. * 1.

      ! Melt any frozen slurry
      HHadj = HH + 1000. * massFrozen * hfSlurry
      massFrozen = 0.0

      ! Potential temperature change (omitting latent energy for now)
      dTemp = - HHadj / (1000. * cpSlurry * massSlurry)

      ! Freeze and thaw
      IF (tempSlurry + dTemp .LT. 0.0) THEN
        ! Use HH to get to 0 C
        HHadj = HHadj + (0.0 - tempSlurry) * 1000. * cpSlurry * massSlurry
        tempSlurry = 0.0

        IF (HHadj .GT. 0.0) THEN
          ! Still some cooling available for freezing
          massFrozen = HHadj / hfSlurry / 1000.
          IF (massFrozen .GE. massSlurry) THEN
            ! More than enough to freeze all slurry
            massFrozen = massSlurry
            HHadj = HHadj - 1000. * massFrozen * hfSlurry
          ELSE
            ! Only some freezes and temperature stays at 0C
            ! massFrozen from above applies
            HHadj = 0.0
          END IF
        END IF
      END IF

      !!! Weighted cp
      !!! Not implemented because cp needs to be used below in tempSS calc and I have to think about what makes sense
      !!cpSlurry = massFrozen / massSlurry * cpFrozen + (1.0 - massFrozen / massSlurry) * cpLiquid

      ! Recalculate dT, now actual temperature change
      dTemp = - HHadj / (1000. * cpSlurry * massSlurry)
      tempSlurry = tempSlurry + dTemp
      
      ! Steady-state temperature
      tempSS = (tempAir(DOY)/Rtop*areaAir + tempFloor(DOY)/Rfloor*areaFloor + tempWall(DOY)/Rdwall*areaDwall + &
        & tempAir(DOY)/Ruwall*areaUwall + (1000. * slurryProd / 86400. * cpSlurry * tempIn) - Qrad) / &
        & (areaAir/Rtop + areaFloor/Rfloor + areaDwall/Rdwall + areaUwall/Ruwall + (1000. * slurryProd / 86400. * cpSlurry))

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

    WRITE(10,"(1X,I4,5X,I3,5X,I4,1X,8F8.2)") DOS, DOY, YR, massSlurry, massFrozen, slurryDepth, tempAir(DOY), & 
        & tempWall(DOY), tempFloor(DOY), sumTempSlurry/24

    WRITE(11,"(1X,I4,5X,I3,5X,I4,1X,8F11.3,4F15.0,1X,1F5.2,2X,L5)") DOS, DOY, YR, Qrad, Qslur2air, Qslur2floor, Qslur2dwall, &
        & Qslur2uwall, Qfeed, Qout, sumQout/24., HH, sumHH/24., HHadj, sumHHadj/24., tempSS, useSS

    WRITE(12,"(1X,I4,5X,I3,5X,I4,1X,2F15.0)") DOS, DOY, YR, tempAir(DOY), solRad(DOY)
    
  END DO

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Close files
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CLOSE(2)
  CLOSE(10)
  CLOSE(11)

  STOP
END PROGRAM stm
