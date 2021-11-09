PROGRAM stm

  ! Simple temperature model for slurry in channels or pits in a barn
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
  INTEGER :: DOY         ! Day of year (1 - 365)
  INTEGER :: YR          ! Relative year (1 + )
  INTEGER :: targetDOY   ! Day of year in target_temp.txt file
  INTEGER :: nextTargetDOY   ! Day of year in target_temp.txt file
  INTEGER :: DOS         ! Day of simulation
  INTEGER :: nDays       ! Number of days in simulation
  INTEGER :: startingDOY ! Starting day of year
  INTEGER :: hottestDOY  ! Hottest day of year
  INTEGER :: raddestDOY  ! Day of year with most radiation
  INTEGER :: emptyDOY1   ! 
  INTEGER :: emptyDOY2   ! 
  INTEGER :: wallAvePeriod, floorAvePeriod ! Number of days in running averages for wall and floor temperature
  INTEGER :: fileStat    ! End of file indicator of target_temp.txt

  ! Simulation ID
  CHARACTER (LEN=4) :: ID ! ID code of simulation
  !!CHARACTER (LEN=1) :: ventType ! Type of ventilation

  ! File names
  CHARACTER (LEN=30) :: userParFile, parFile, weatherFile
  INTEGER :: numArgs

  ! Temperatures, all in degrees C
  REAL, DIMENSION(365) :: tempAir, tempFloor, tempWall ! Air, floor (bottom of channel or pit), and wall (side of channel or pit)
  REAL :: tempSum, dTemp
  REAL :: tempTarget     ! Target temperature for a particular period when heated
  REAL :: nextTempTarget     ! Target temperature for a particular period when heated
  REAL :: maxAnnTemp     ! Maximum daily average air temperature over the year
  REAL :: minAnnTemp     ! Minimum daily average air temperature over the year
  REAL :: tempInitial    ! Initial slurry temperature
  REAL :: tempSlurry     ! Hourly or daily slurry temperature 
  REAL :: tempSlurryH0   ! Slurry temperature at start of day (used for Qfeed calc)
  REAL :: tempSS         ! Steady-state slurry temperature
  REAL :: sumTempSlurry  ! Sum of hourly slurry temperatures for calculating daily mean
  REAL :: tempIn         ! Temperature of slurry when added to channel/pit
  REAL :: trigPartTemp   ! Sine part of temperature expression
  REAL :: residMass      ! Mass of slurry left behind when emptying

  ! Geometry of channel or pit
  REAL :: slurryDepth    ! Depth of slurry in channel/pit (m)
  REAL :: channelDepth   ! Total depth of channel/pit (m)
  REAL :: buriedDepth    ! Buried depth (m)
  REAL :: wallDepth      ! Depth at which horizontal heat transfer is evaluated (m) 
  REAL :: length, width  ! Length and width of channel/pit (m)
  REAL :: areaFloor      ! Channel or pit floor area in contact with soil (m2)
  REAL :: areaDwall   ! Channel or pit wall area in contact with soil (buried) (m2)
  REAL :: areaUwall    ! Channel or pit wall area in contact with air (above ground) (outside) and slurry (inside) (m2)
  !REAL :: areaSurf      ! Slurry upper surface area (m2)
  REAL :: areaAir        ! 
  INTEGER :: nChannels   ! Number of channels or pits

  ! Solar
  REAL :: areaSol        ! 
  REAL :: absorp         !
  REAL :: maxAnnRad      ! Maximum daily average radiation over the year
  REAL :: minAnnRad      ! Minimum daily average radiation over the year
  REAL, DIMENSION(365) :: solRad
  REAL :: trigPartRad    ! Sine part of radiation expression
  
  ! Other slurry variables
  REAL :: massSlurry, massFrozen = 0, dmassFrozen   ! Slurry mass (kg) NTS: actually Mg = t!
  REAL :: slurryVol      ! Initial slurry volume (m3) NTS not consistent name
  REAL :: slurryProd     ! Slurry production rate (= inflow = outflow) (tonnes/d)

  ! Heat transfer coefficients and related variables
  REAL :: uAir, uUwall, uDwall, uFloor, uTop          ! Convective heat transfer coefficient W/m2-K (J/s-m2-K) 
  REAL :: kSlur          ! Thermal conductivity of slurry in W/m-K
  REAL :: kConc          ! Thermal conductivity of concrete in W/m-K
  REAL :: kSoil          ! Thermal conductivity of soil in W/m-K
  REAL :: cpConc         ! Heat capacity of concrete J/kg-K
  REAL :: cpSlurry       ! Heat capacity of slurry J/kg-K
  REAL :: hfSlurry       ! Latent heat of fusion of slurry J/kg
  REAL :: dConc          ! Density of concrete kg/m3
  REAL :: dSlurry        ! Density of slurry kg/m3
  REAL :: glConc         ! Gradient length within concrete substrate in m
  REAL :: glSlur         ! Gradient length within slurry in m
  REAL :: glSoil         ! Gradient length within soil (below floor and beside walls) in m
  REAL :: soilDamp       ! Soil damping depth, where averaging period reaches 1 full yr, in m

  ! Heat flux variables in W/m2 out of slurry
  REAL :: Qrad           ! "To" sun
  REAL :: Qslur2air      ! To air
  REAL :: Qslur2wall, Qslur2dwall, Qslur2uwall   ! To wall
  REAL :: Qslur2floor    ! To floor
  !REAL :: Qslur2eff      ! To effluent (includes flow in)
  REAL :: Qfeed          ! Loss due to feeding
  REAL :: Qout           ! Total

  LOGICAL :: calcWeather ! .TRUE. when weather inputs are calculated (otherwise read from file)

  ! Other parameters
  REAL, PARAMETER :: PI = 3.1415927
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
  ELSE IF (numArgs .EQ. 4) THEN
    CALL GET_COMMAND_ARGUMENT(1, ID)
    CALL GET_COMMAND_ARGUMENT(2, parFile)
    CALL GET_COMMAND_ARGUMENT(3, userParFile)
    CALL GET_COMMAND_ARGUMENT(4, weatherFile)
    calcWeather = .FALSE.
  ELSE
    WRITE(*,*) 'No file names given so 2 default parameter files will be used, with calculated weather and ID 0001.'
    parFile = 'pars.txt'
    userParFile = 'user_pars.txt'
    ID = '0001'
    calcWeather = .TRUE.
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
  !!!OPEN (UNIT=3, FILE='target_temp.txt', STATUS='OLD')

  ! Output files, name based on ID
  OPEN (UNIT=10,FILE=(''//ID//'_temp.txt'),STATUS='UNKNOWN')
  OPEN (UNIT=11,FILE=(''//ID//'_rates.txt'),STATUS='UNKNOWN')
  OPEN (UNIT=12,FILE=(''//ID//'_weather.txt'),STATUS='UNKNOWN')
  !OPEN (UNIT=12,FILE='pars'//ID//'.txt',STATUS='UNKNOWN')
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Read parameters
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! User parameters
  READ(1,*)
  READ(1,*) nDays
  READ(1,*) startingDOY
  READ(1,*) nChannels
  READ(1,*) channelDepth
  READ(1,*) buriedDepth
  READ(1,*) length
  READ(1,*) width
  READ(1,*) areaAir
  READ(1,*) areaSol
  READ(1,*) slurryVol
  READ(1,*) tempInitial
  READ(1,*) minAnnTemp
  READ(1,*) maxAnnTemp 
  READ(1,*) hottestDOY
  READ(1,*) minAnnRad
  READ(1,*) maxAnnRad 
  READ(1,*) raddestDOY
  READ(1,*) slurryProd
  READ(1,*) residMass
  READ(1,*) tempIn
  READ(1,*) emptyDOY1
  READ(1,*) emptyDOY2 
  !!!READ(1,*) ventType

  ! Other parameters
  READ(2,*) 
  READ(2,*) uAir
  READ(2,*) kSlur
  READ(2,*) kConc
  READ(2,*) kSoil
  READ(2,*) cpConc
  READ(2,*) cpSlurry
  READ(2,*) hfSlurry
  READ(2,*) dConc
  READ(2,*) dSlurry
  READ(2,*) glSlur
  READ(2,*) glConc
  READ(2,*) glSoil
  READ(2,*) absorp
  READ(2,*) soilDamp

  ! Output file header
  WRITE(10,*) 'Day of  Day of    Year   Slurry  Frozen  Slurry   Air    Wall   Floor    Slurry'
  WRITE(10,*) 'sim.     year             mass    mass   depth     T      T       T        T'

  WRITE(11,*) 'Day of  Day of Year ' 
  WRITE(11,*) 'sim.     year               Qrad         Qslur2air     Qslur2floor      Qslur2dwall      Qslur2uwall       Qout' 

  WRITE(12,*) 'Day of  Day of Year Air   Radiation'
  WRITE(12,*) 'sim.     year        T'
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Initial calculations
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Calculate some geometry-related variables
  wallDepth = 0.5 * buriedDepth
  areaFloor = width*length*nChannels
  !areaSurf = width*length*nChannels
  massSlurry = slurryVol * dSlurry / 1000 ! Slurry mass is in tonnes = 1000 kg

  ! Heat transfer coefficients
  uFloor = 1./(glConc/kConc + glSlur/kSlur)
  uDwall = 1./(glSoil/kSoil + glConc/kConc + glSlur/kSlur)
  uUwall = 1./(1./uAir + glConc/kConc + glSlur/kSlur)

  ! Adjust u for air by gl in slurry
  uTop = 1./(1./uAir + glSlur/kSlur)

  ! Determine airTemp, solRad, and substrate temperatures for a complete year
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
  ELSE 
    ! Skip header
    READ(3,*)
    DO WHILE (.NOT. IS_IOSTAT_END(fileStat))
      READ(3,*,IOSTAT=fileStat) DOY, tempAir(DOY), solRad(DOY)
    END DO
  END IF

  ! Substrate temperature based on moving average
  ! NTS: how about aveperiods > 365?
  wallAvePeriod = wallDepth/soilDamp*365
  floorAvePeriod = buriedDepth/soilDamp*365

  ! Calculate moving average for first day of year
  ! Wall first
  IF (wallAvePeriod > 365) THEN
    DO DOY = 1,365,1
      tempWall(DOY) = (minAnnTemp + maxAnnTemp)/2.
    END DO
  ELSE 
    ! First need to calculate value for DOY = 1
    tempWall(1) = 0
    DO DOY = 365 - wallAvePeriod + 2,365,1
      tempWall(1) = tempWall(1) + tempAir(DOY)/wallAvePeriod
    END DO

    DO DOY = 2,365,1
      IF (DOY > wallAvePeriod) THEN
        tempWall(DOY) = tempWall(DOY - 1) - tempAir(DOY - wallAvePeriod)/wallAvePeriod + tempAir(DOY)/wallAvePeriod
      ELSE 
        tempWall(DOY) = tempWall(DOY - 1) - tempAir(365 + DOY - wallAvePeriod)/wallAvePeriod + tempAir(DOY)/wallAvePeriod
      END IF 
    END DO
  END IF

  ! Then floor
  IF (floorAvePeriod > 365) THEN
    DO DOY = 1,365,1
      tempFloor(DOY) = (minAnnTemp + maxAnnTemp)/2.
    END DO
  ELSE 
    ! First need to calculate value for DOY = 1
    tempFloor(1) = 0
    DO DOY = 365 - floorAvePeriod + 2,365,1
      tempFloor(1) = tempFloor(1) + tempAir(DOY)/floorAvePeriod
    END DO

    DO DOY = 2,365,1
      IF (DOY > floorAvePeriod) THEN
        tempFloor(DOY) = tempFloor(DOY - 1) - tempAir(DOY - floorAvePeriod)/floorAvePeriod + tempAir(DOY)/floorAvePeriod
      ELSE 
        tempFloor(DOY) = tempFloor(DOY - 1) - tempAir(365 + DOY - floorAvePeriod)/floorAvePeriod + tempAir(DOY)/floorAvePeriod
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

    ! Empty and add slurry at beginning of day
    IF (DOY == emptyDOY1 .OR. DOY == emptyDOY2) THEN
      massSlurry = residMass
      IF (massFrozen .GT. massSlurry) THEN
        massFrozen = massSlurry
      END IF
    END IF

    ! Update slurry mass assuming instantaneous slurry addition once per day
    massSlurry = massSlurry + slurryProd
    tempSlurryH0 = tempSlurry
    ! Temperature effect of adding slurry is added hourly below

    ! Get depth and wall area
    slurryDepth = 1000 * massSlurry / (dSlurry*width*length*nchannels)  ! Slurry depth in m
    areaDwall = MIN(slurryDepth, buriedDepth) * 2. * (length + width) * nChannels
    areaUwall = MAX(slurryDepth - buriedDepth, 0.) * 2. * (length + width) * nChannels

    ! Start hour loop
    sumTempSlurry = 0

    ! Radiation fixed for day (W = J/s)
    Qrad = - absorp * solRad(DOY) * areaSol

    DO HR = 1,24,1
    
      ! Calculate heat transfer rates, all in watts (J/s)
      Qfeed = (tempSlurryH0 - tempSlurry) * 1000 * slurryProd / 24. / 3600. * cpSlurry
      Qslur2air = uTop*(tempSlurry - tempAir(DOY))*areaAir
      Qslur2floor = uFloor*(tempSlurry - tempFloor(DOY))*areaFloor
      Qslur2dwall = uDwall*(tempSlurry - tempWall(DOY))*areaDwall
      Qslur2uwall = uUwall*(tempSlurry - tempAir(DOY))*areaUwall
      Qslur2wall = Qslur2dwall + Qslur2uwall
      Qout = Qfeed + Qrad + Qslur2air + Qslur2wall + Qslur2floor
      dTemp = - Qout*3600./(1000*cpSlurry*massSlurry)

      ! Freezing
      IF (tempSlurry .LT. 0.01) THEN
        dmassFrozen = Qout / hfSlurry / 1000 * 3600
        IF (dmassFrozen .GT. 0. .AND. massFrozen + dmassFrozen .GT. massSlurry) THEN
          ! Some heat loss goes toward freezing all slurry
          massFrozen = massSlurry
          dTemp = - (Qout - (massSlurry - massFrozen) * hfSlurry) * 3600. / (1000*cpSlurry*massSlurry)
        ELSE IF (dmassFrozen .GT. 0.) THEN
          ! All heat loss goes toward freezing some slurry
          massFrozen = massFrozen + dmassFrozen
          dTemp = 0
        END IF
      END IF

      ! Melting
      IF (massFrozen .GT. 0 .AND. Qout .LT. 0) THEN
        dmassFrozen = Qout / hfSlurry / 1000 * 3600
        IF (ABS(dmassFrozen) .GT. massFrozen) THEN
          ! Some heat gain goes toward melting all frozen slurry
          massFrozen = 0
          dTemp = - (Qout - (massFrozen) * hfSlurry) * 3600. / (1000*cpSlurry*massSlurry)
        ELSE
          ! All heat gain goes toward melting some frozen slurry
          massFrozen = massFrozen + dmassFrozen
          dTemp = 0
        END IF
      END IF
      
      ! Steady-state temperature
      tempSS = (uTop*tempAir(DOY)*areaAir + uFloor*tempFloor(DOY)*areaFloor + uDwall*tempWall(DOY)*areaDwall + &
        & uUwall*tempWall(DOY)*areaUwall - Qrad) / (uTop*areaAir + uFloor*areaFloor + uDwall*areaDwall + uUwall*areaUwall)

      tempSlurry = tempSlurry + dTemp
      
      ! Limit change in temperature to change to SS temp
      IF (dTemp > 0. .AND. tempSlurry > tempSS) THEN
        tempSlurry = tempSS
      ELSEIF (dTemp < 0. .AND. tempSlurry < tempSS) THEN
        tempSlurry = tempSS
      END IF
      
      sumTempSlurry = sumTempSlurry + tempSlurry

    END DO


    WRITE(10,"(1X,I4,5X,I3,5X,I4,1X,7F8.2)") DOS, DOY, YR, massSlurry, massFrozen, slurryDepth, tempAir(DOY), & 
        & tempWall(DOY), tempFloor(DOY), sumTempSlurry/24. 

    WRITE(11,"(1X,I4,5X,I3,5X,I4,1X,6F15.0)") DOS, DOY, YR, Qrad, Qslur2air, Qslur2floor, Qslur2dwall, Qslur2uwall, Qout

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
