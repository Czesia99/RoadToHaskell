# Wolfram in Haskell

<p align="center">
<img src="https://en.wikipedia.org/wiki/Cellular_automaton#/media/File:CA_rule30s.png" alt="wolframn"/><br/>
</p>

The Purpose of this project is to make Wolfram's elementary cellular automaton

https://en.wikipedia.org/wiki/Cellular_automaton

## Usage:

```
git clone https://github.com/CzesiaLa/RoadToHaskell.git
cd RoadToHaskell/
cd 03-wolfram/
make
```

| Arguments | Effect                                                                       |
|-----------|------------------------------------------------------------------------------|
| --rule    | the ruleset to use (0 to 255)                                                |
| --start   | the generation number at which to start the display. The default value is 0. |
| --lines   | the number of lines to display. When homited, the program never stops.       |        
| --window  | the number of cells to display on each line (default 80)                     |
| --move    | NOT IMPLEMENTED                                                              |


### Example:

```
./wolfram --rule 30 --lines 20

                                        *                                       
                                       ***                                      
                                      **  *                                     
                                     ** ****                                    
                                    **  *   *                                   
                                   ** **** ***                                  
                                  **  *    *  *                                 
                                 ** ****  ******                                
                                **  *   ***     *                               
                               ** **** **  *   ***                              
                              **  *    * **** **  *                             
                             ** ****  ** *    * ****                            
                            **  *   ***  **  ** *   *                           
                           ** **** **  *** ***  ** ***                          
                          **  *    * ***   *  ***  *  *                         
                         ** ****  ** *  * *****  *******                        
                        **  *   ***  **** *    ***      *                       
                       ** **** **  ***    **  **  *    ***                      
                      **  *    * ***  *  ** *** ****  **  *                     
                     ** ****  ** *  ******  *   *   *** ****                    
                    **  *   ***  ****     **** *** **   *   *                   
```