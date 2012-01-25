import SOE
main
   = runGraphics $
     do w <- openWindow "My First Graphics Program" (300, 300)
        drawInWindow w (text (100, 200) "HelloGraphicsWorld")
        k <- getKey w
        closeWindow w
