
      ! Update slurry temperature
      tempSlurry = tempSlurry + dTemp

      ! Freeze/thaw
      !                     J        /     J/t
      !              --------------     ------------
      !    t         J/s * s/h  * h  / J/kg   / kg/t 
      dmassFrozen = Qout * 3600. * 1./ hfSlurry / 1000. 
      IF (tempSlurry + dTemp .LT. 0.0) THEN ! Freeze some slurry
        IF (dmassFrozen .GT. 0.0 .AND. massFrozen + dmassFrozen .GT. massSlurry) THEN

        END IF

      ELSE IF (massFrozen .GT. 0.0) THEN    ! Possibly thaw some slurry

      END IF


      IF (tempSlurry .LT. 0.01) THEN
        !    t         J/s * s/h  * h/   J/kg   * kg/t 
        dmassFrozen = Qout * 3600. * 1./ hfSlurry / 1000. 
        IF (dmassFrozen .GT. 0. .AND. massFrozen + dmassFrozen .GT. massSlurry) THEN
          ! Some heat loss goes toward freezing all slurry
          massFrozen = massSlurry
          !                                      J/s
          !           ---------------------------------------------------------------
          ! J/s      J/s    kg/t         t            t          J/kg       s/h  * h   
          QoutPart = Qout - 1000 * (massSlurry - massFrozen) * hfSlurry /  3600. / 1.
          dTemp = - QoutPart * 3600./(1000 * cpSlurry * massSlurry) * 1
        ELSE IF (dmassFrozen .GT. 0.) THEN
          ! All heat loss goes toward freezing some slurry
          massFrozen = massFrozen + dmassFrozen
          dTemp = 0
        END IF
      END IF

      ! Melting
      IF (massFrozen .GT. 0 .AND. Qout .LT. 0) THEN
        !    t         J/s /   J/kg   / kg/t * s/h  * h
        dmassFrozen = Qout / hfSlurry / 1000 * 3600 * 1
        IF (-dmassFrozen .GT. massFrozen) THEN
          ! Some heat gain goes toward melting all frozen slurry
          massFrozen = 0
          QoutPart = Qout + 1000 * massFrozen * hfSlurry /  3600. / 1.
          dTemp = - QoutPart * 3600./(1000 * cpSlurry * massSlurry) * 1
        ELSE
          ! All heat gain goes toward melting some frozen slurry
          massFrozen = massFrozen + dmassFrozen
          dTemp = 0
        END IF
      END IF

