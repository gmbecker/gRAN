##NOT run in unit tests or vignette because it is too expensive for us in time
##and Bioc in bandwidth to be repeatedly run



if(!file.exists("~/permtemp"))
    dir.create("~/permtemp")


res = locatePkgVersion("BiocInstaller", "1.14.1", dir = "~/permtemp")
