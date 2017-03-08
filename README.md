# HashCashUtilities
CLI functionality around the HashCash algorithm written in Haskell.

HashCash Command Line Interface Tool: Copyright 2017 Dustin Briscoe"

This command line tool will allow you to mint a valid HashCash header, or validate it.  The following tool is to be 
used at your own risk.  It is intended to be a proof of concept in implementing a proof of work scheme in a 
purely functional language like Haskell.

Features not yet implemented:
 - Only version 1 of HashCash is implemented
 - Timestamp validation is not currently occurring

  Commands: 
    generate - Create a valid X-Hashcash mail header given various options
      -v [option] = Use a specific email address or other value
      -d [option] = Use a specific valid timestamp such that timestamp of mail header can be validated
      -p [option] = Modify the difficulty setting of the proof of work (NOTE: Higher numbers require more work)
      -t [option] = Number of parallel threads of computation.  On multicore systems this may result in faster 
                    generation of a header.
    validate - Validate an X-Hashcash mail header and return 'Valid' or 'Not Valid'
