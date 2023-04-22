import "./style.css";
import { fromEvent, interval, Observable, of, range,merge} from 'rxjs'; 
import { map, filter, scan, mergeMap,takeUntil} from 'rxjs/operators';

function main() {
  /**
   * Inside this function you will use the classes and functions from rx.js
   * to add visuals to the svg element in pong.html, animate them, and make them interactive.
   *
   * Study and complete the tasks in observable examples first to get ideas.
   *
   * Course Notes showing Asteroids in FRP: https://tgdwyer.github.io/asteroids/
   *
   * You will be marked on your functional programming style
   * as well as the functionality that you implement.
   *
   * Document your code!
   */

  /**
   * This is the view for your game to add and update your game elements.
   */
  const svg = document.querySelector("#svgCanvas") as SVGElement & HTMLElement;

  //Immutable vector class
  class Vec {
    constructor(public readonly x: number = 0, public readonly y: number = 0) {}
    add = (b:Vec) => new Vec(this.x + b.x, this.y + b.y)
    sub = (b:Vec) => this.add(b.scale(-1))
    len = ()=> Math.sqrt(this.x*this.x + this.y*this.y)
    scale = (s:number) => new Vec(this.x*s,this.y*s)
    
    static Zero = new Vec();
  }
  
  //Constants value for game
  const Constants = { 
    //Canvas
    CanvasSizeX: 880,
    CanvasSizeY: 880,
    //River section
    RiverStartPosY: 240,
    RiverEndPosY: 480,
    //Body: Car, Plank, Crocodile, target box
    BodyCountPerRow: 2,
    GapBetweenBody: 250,
    CarHeight: 50,
    CarWidth: 60,
    NumberOfCarRow: 3,
    CarStartPosY: 730,
    PlankHeight: 50,
    PlankWidth: 200,
    NumberOfPlankRow: 2,
    PlankStartPosY: 410,
    CrocHeight: 50,
    CrocWidth: 120,
    NumberOfCrocRow: 1,
    CrocStartPosY: 250,
    TargetBoxHeightAndWidth: 80,
    NumberOfTargetBox: 5,
    TargetBoxStartPosY: 170,
    //Level
    StartingLevel: 1,
  } as const

  ///////////////////////////////////////////////Handle Input//////////////////////////////////////////////////////

  //5 types of game transition
  class Tick { constructor(public readonly elapsed:number) {} }
  class RightMove { constructor(public readonly direction:Vec, public readonly rotate:number) {} }
  class LeftMove { constructor(public readonly direction:Vec, public readonly rotate:number) {} }
  class UpMove { constructor(public readonly direction:Vec, public readonly rotate:number) {} }
  class DownMove { constructor(public readonly direction:Vec, public readonly rotate:number) {} }
  class Restart { constructor() {}}

  //Functions for observing keys and tick

  //Observable stream of Tick instance
  const gameClock = interval(10).pipe(map(elapsed=>new Tick(elapsed)))
  
  type Key = 'ArrowLeft' | 'ArrowRight' | 'ArrowUp' | 'ArrowDown' | 'Space'
  type Event = 'keydown' | 'keyup'

  //Function which returns obsevable stream of RightMove or LeftMove or UpMove or DownMove instance
  const observeKey = <T>(eventName:Event, k:Key, result:()=>T):Observable<T>=>
    fromEvent<KeyboardEvent>(document,eventName)
      .pipe( //prevent default needed 
        filter(({code})=>code === k),
        filter(({repeat})=>!repeat),
        map(result))

  //Obsevable stream created by calling observeKey function
  const
    rightMove = observeKey('keydown','ArrowRight',()=>new RightMove(new Vec(80,0),90)),
    leftMove = observeKey('keydown','ArrowLeft',()=>new LeftMove(new Vec(-80,0),-90)),
    upMove = observeKey('keydown','ArrowUp',()=>new UpMove(new Vec(0,-80),0)),
    downMove = observeKey('keydown','ArrowDown',()=>new DownMove(new Vec(0,80),180)),
    restart = observeKey('keydown','Space',()=>new Restart())


  //view element type of the game
  type ViewType = 'car' | 'plank' | 'target' | 'crocodile' | 'fly'

  ////////////////////////////////////////////////////Frog//////////////////////////////////////////////////////////
  //Frog type
  type Frog = Readonly<{
    pos: Vec,
    vel: Vec,
    mid: Vec, //middle point of frog
    width: number,
    height: number,
    rotate: number
  }>

  //Function for creating a frog in starting position
  const createFrog = () => 
    <Frog> { 
      pos: new Vec(410,810),
      vel: Vec.Zero,
      mid: new Vec(205,810),
      width: 60,
      height:45,
      rotate: 0
    }

  ////////////////////////////////////////////////////Body//////////////////////////////////////////////////////////
  //Body type - for cars, planks, crocodile, fly 
  type Body = Readonly<{
    id: string,
    pos: Vec,
    mid: Vec, //middle point of the body
    vel: Vec,
    width: number,
    height: number,
    colFac: number, //collision factor
    viewType: ViewType
  }>

  //Curried function, for creating body type
  const createBody = (w:number) => (h:number) => (colFac:number) => (viewType: ViewType) => (row:number) => (oid: number) => (x:number) => (y:number) => (vel_x:number) =>
    <Body> {
      id: viewType+row+oid, 
      pos: new Vec(x,y),
      mid: new Vec(x+(w/2),y),
      vel: new Vec(vel_x,0),
      width: w,
      height: h,
      colFac: colFac, 
      viewType: viewType
    }

  //Partially applied curried function
  //Curried function createBody allows some of the parameters (in this case row, oid, x and y position) to be pass later on
  //passing function into other function allows us to reuse the code
  const createCar = createBody(Constants.CarWidth)(Constants.CarHeight)(Constants.CarWidth)('car')
  const createPlank = createBody(Constants.PlankWidth)(Constants.PlankHeight)(Constants.PlankWidth/2)('plank')
  const createCroc = createBody(Constants.CrocWidth)(Constants.CrocHeight)(Constants.CrocWidth/2)('crocodile')
  const createTarget = createBody(Constants.TargetBoxHeightAndWidth)(Constants.TargetBoxHeightAndWidth)(Constants.TargetBoxHeightAndWidth/2)('target')

  //type aliases for the createCar, createPlank, createCroc, createTarget functions
  type create = (row: number) => (oid: number) => (x: number) => (y: number) => (vel_x: number) => Body

  //Create an array of body by passing in the functions such as createCar, createPlank, CreateCroc and createTarget as the parameter f.
  //complete the partially applied curried function in createCar, createPlank, CreateCroc and createTarget
  const Create_body_array = (n_of_row:number, f:create, y_start_pos: number, vel:number, acc: Array<Body>=[]):Array<Body> => {
    return (n_of_row!== 0) ? (n_of_row%2!==0) ? Create_body_array(n_of_row-1,f,y_start_pos,vel,acc.concat([...Array(Constants.BodyCountPerRow)].map((_,i)=>f(n_of_row)(i)(Constants.GapBetweenBody*i)(y_start_pos-80*(n_of_row-1))(vel)))) : 
      Create_body_array(n_of_row-1,f,y_start_pos,vel,acc.concat([...Array(Constants.BodyCountPerRow)].map((_,i)=>f(n_of_row)(i)(Constants.GapBetweenBody*i)(y_start_pos-80*(n_of_row-1))(-vel-2)))):  acc
  }

  //Create an array of target boxes
  const Create_target_array = (n:number):Array<Body> => {
    return [...Array(n)].map((_,i)=>createTarget(0)(i)(80+(160*(i)))(Constants.TargetBoxStartPosY)(0))
  }

  /////////////////////////////////////////////////Game State//////////////////////////////////////////////////

  //Game State
  //Multiple instance of deeply immutable state is needed, hence we declare immutable types using the Readonly
  type State = Readonly<{
    time: number,
    frog: Frog,
    cars:ReadonlyArray<Body>,
    fly: Body,
    flyEaten: Boolean, //notify the updateView function to hide the fly after it is eaten by frog
    crocodile: ReadonlyArray<Body>,
    plank: ReadonlyArray<Body>,
    targetBox: ReadonlyArray<Body>,
    utilisedTargetBox: Body|null, //notify the updateView function to unhide the frog on target box - filling the box
    gameOver: Boolean,
    win: Boolean,
    furthestY: number, //Used to compared with corrent Y position of frog, increment score if frog travel fruther than this value
    score: number,
    highestScore: number, //Value is kept even after gameover or restart
    lives: number, 
    level: number //level of state is used to decide the the velocity of cars and plank (higher level, greater difficulty by increasing the velocity of car and plank)
  }>

  //initialState which is an instance of State type
  const initialState: State = { 
    time: 0,
    frog: createFrog(),
    cars: Create_body_array(Constants.NumberOfCarRow,createCar,Constants.CarStartPosY,Constants.StartingLevel), //after winning, can manually set the max value of velocity to higher number by adding a new parameters
    fly: createBody(60)(50)(60)('fly')(1)(1)(30)(410)(1), //change to constant
    plank: Create_body_array(Constants.NumberOfPlankRow,createPlank,Constants.PlankStartPosY,Constants.StartingLevel),
    crocodile: Create_body_array(Constants.NumberOfCrocRow,createCroc,Constants.CrocStartPosY,Constants.StartingLevel),
    targetBox: Create_target_array(Constants.NumberOfTargetBox),
    utilisedTargetBox: null,
    gameOver: false,
    win: false,
    furthestY: 810,
    score: 0,
    flyEaten: false,
    highestScore: 0,
    lives: 3,
    level: Constants.StartingLevel
  } as const

  // wrap a positions around edges of the screen along x-axis
  const torusWrapX = ({x,y}:Vec) => (objectWidth:number) => { 
    const wrap_x = x < -objectWidth ? Constants.CanvasSizeX : x > Constants.CanvasSizeX ? -objectWidth : x;
    return new Vec(wrap_x,y)
  };

  //Limit the movement along Y axis
  const limitY = ({x,y}:Vec) => { 
    const limit_y =  y < 0 ? y+80 : y > Constants.CanvasSizeY ? y-80  : y;
    return new Vec(x,limit_y)
  };

  //Function for moving Body
  const moveObj = (o:Body)  => <Body> {
    ...o,
    pos: torusWrapX(o.pos.add(o.vel))(o.width),
    mid: o.pos.add(new Vec(o.width/2,0))
  }

  //Function for moving Frog
  const moveFrog = (o:Frog)  => <Frog> {
    ...o,
    pos: torusWrapX(o.pos.add(o.vel))(o.width),
    mid: o.pos.add(new Vec(o.width/2,0))
  }

  ///////////////////////////////////////////////Manipulate state data////////////////////////////////////////////////////

  //Function handle collision with car, frog drowning in water (frog not on plank or crocodile), 
  //as well as landing in prohibited area (area between the target box)
  //Function also handle increment of score when frog land on distinct target areas
  const handleCollisionsAndScore = (s:State) => {
    const
      // returns true if body collided with frog
      bodiesCollided = ([a,b]:[Frog,Body]) => a.pos.y === b.pos.y && a.mid.sub(b.mid).len() < b.colFac,
      // notbodiesCollided = ([a,b]:[Frog,Body]) => a.pos.y === b.pos.y && a.mid.sub(b.mid).len() > b.colFac,
      // returns true if 1 or more car collided with frog
      carCollided = s.cars.filter(c=>bodiesCollided([s.frog,c])).length > 0,
      // returns true if frog landed plank or crocodile 
      plankOrCrocCollided = s.plank.concat(s.crocodile).find(p=>bodiesCollided([s.frog,p])),
      // returns true if frog landed in river
      notOnPlankOrCroc = s.frog.pos.y>Constants.RiverStartPosY && s.frog.pos.y<Constants.RiverEndPosY && !plankOrCrocCollided? true:false,
      // returns true if frog consumed the fly
      flyCollided = bodiesCollided([s.frog,s.fly]),
      // returns the target box in which frog landed on
      targetCollided = s.targetBox.find(p=>bodiesCollided([s.frog,p])),
      // returns an array of target in which frog did not land on
      targetLeft:Body[] = s.targetBox.filter(p=>!bodiesCollided([s.frog,p])),
      // returns true if frog lands in between target box
      notOnTarget = s.frog.pos.y===Constants.TargetBoxStartPosY && !(targetCollided)? true:false,
      // returns true if frog collided with obstacles or drown in river or landed on area between target box
      frogDies = carCollided || notOnPlankOrCroc || notOnTarget
  
      return <State>{
        ...s,
        //frog position goes back to initial position if frog land on target area or frog dies
        //frog velocity is set to plank or crocodile velocity if frog landed on them
        //frog rotation angle is set back to initial state's 0 f frog land on target area or frog dies
        frog: {...s.frog, 
          pos:(targetCollided|| frogDies) ? initialState.frog.pos:s.frog.pos, 
          vel: plankOrCrocCollided ? new Vec(plankOrCrocCollided.vel.x,0): Vec.Zero,
          rotate: (targetCollided|| frogDies) ? initialState.frog.rotate : s.frog.rotate},
        //when frog land on target box, update leftover target box (remove the landed target box from array)
        targetBox: targetCollided?targetLeft:s.targetBox,
        //the target box in which frog landed on, in order to update view 
        utilisedTargetBox: targetCollided,
        //when frog land on target box, reset the furthest y position to the initial y value (so frog can continue scoring)
        furthestY: targetCollided? initialState.furthestY:s.furthestY,
        //when frog land on target box, increment score by 100
        score: targetCollided? s.score+100:s.score,
        //if fly eaten by frog, set flyEaten to tru
        flyEaten: flyCollided?true:s.flyEaten,
        //lives reduce by 1 if frog dies
        lives: (frogDies) ? s.lives-1: s.lives,
        //gameOver set to true if live is 0
        gameOver: s.lives < 1 ? true : s.gameOver,
        //win set to true with all target box is filled
        win: s.targetBox.length===0
      }
  }

  //Interval tick: Bodies and frog move (and handle collisions), check for winning or gameover condition
  const tick = (s:State,elapsed:number) => {
    //If user filled up 5 target box (win), return a new state 
    if (s.win) {
      return <State> {
        ...s,
        cars: Create_body_array(Constants.NumberOfCarRow,createCar,Constants.CarStartPosY,s.level+1),
        plank: Create_body_array(Constants.NumberOfPlankRow,createPlank,Constants.PlankStartPosY,s.level+1),
        win: false, 
        targetBox: initialState.targetBox, //Set the target box back to 5
        level: s.level+1 //Increment the level by 1 
      }
    }
    // if (s.gameOver) {
    //   return <State> {...initialState, highestScore: s.score>s.highestScore? s.score:s.highestScore}
    // }
    return handleCollisionsAndScore ({...s,
      //update the time, it is used later for the fly movement so it moves back and forth
      time: elapsed,
      //moving frog and bodies
      frog: moveFrog(s.frog),
      cars: s.cars.map(moveObj),
      plank: s.plank.map(moveObj),
      crocodile: s.crocodile.map(moveObj),
      //fly will swing left and right by setting the the velocity on x-axis from positive to negative and vice versa 
      //fly changes direction every 200ms
      fly: moveObj({...s.fly, vel:s.time%200===0? new Vec(-s.fly.vel.x,s.fly.vel.y):s.fly.vel}),
      //compare the position of frog on y-axis with the furthest y it had achieved,  update the furthest y value accordingly
      furthestY: s.frog.pos.y<s.furthestY? s.frog.pos.y:s.furthestY,
      //if frog travel further than the furthest y value it had achieved, increment score by 2 if fly is eaten, else increment score by 1
      score: s.frog.pos.y<s.furthestY? s.flyEaten ? s.score+2 : s.score+1: s.score 
    })
  }

  //state transducer
  const reduceState = (s:State, e:RightMove|LeftMove|UpMove|DownMove|Restart|Tick):State=>
    //When user press on right key or left key, update the position of the frog
    e instanceof RightMove || e instanceof LeftMove? {...s,
      frog: {...s.frog, 
        pos: torusWrapX(s.frog.pos.add(e.direction))(s.frog.width), 
        mid: s.frog.pos.add(new Vec(s.frog.width/2,0)),
        rotate: e.rotate
      } 
    } :
    //When user press on down key or up key, update the position of the frog
    e instanceof UpMove || e instanceof DownMove ? {...s,
      frog: {...s.frog, 
        pos: limitY(s.frog.pos.add(e.direction)),
        rotate: e.rotate
      }
    } :
    e instanceof Restart ? {...initialState, 
      highestScore: s.score>s.highestScore? s.score:s.highestScore
    }:
    //invoke tick function every 10ms when there is no keyboard event
    tick(s,e.elapsed);

  ////////////////////////////////////////////////////Update View/////////////////////////////////////////////////////////
  function updateView(s:State): void {
    const svg = document.getElementById("svgCanvas")!;

    const updateBodyView = (b:Body) => {
      //reusable createBodyView function
      function createBodyView() {
        if (b.viewType === 'crocodile') {
          const c = document.createElementNS(svg.namespaceURI, "image")!;
          c.setAttributeNS("http://www.w3.org/1999/xlink", "href", "https://www.svgrepo.com/show/53678/crocodile.svg");
          c.setAttribute("id", b.id);
          c.setAttribute("width", String(200));
          svg.appendChild(c);
          return c;
        }
        else {
          const v = document.createElementNS(svg.namespaceURI, "rect")!;
          v.classList.add(b.viewType)
          v.setAttribute("id", b.id)
          svg.appendChild(v)
          v.setAttribute("width", String(b.width));
          v.setAttribute("height", String(b.height));
          return v;
        }
      }
      //Get the id of the body, check if body view is created, 
      //if body view is already created, update the x and y position for view, else create the body view by calling createBodyView() function
      if (b.viewType === 'crocodile') {
        const v = document.getElementById(String(b.id)) || createBodyView();
        v.setAttribute("x", String(b.pos.x));
        v.setAttribute("y", String(b.pos.y-70)); //-70 is needed because crocodile SVG is not nicely align
      }
      else {
        const v = document.getElementById(String(b.id)) || createBodyView();
        v.setAttribute("x", String(b.pos.x));
        v.setAttribute("y", String(b.pos.y));
      }
    };

    //Use updateBodyView function for each body in the cars, planks, crocodile array
    //Update the position of the Body (cars, plank, crocodile, fly) so it moves across screen
    s.cars.forEach(updateBodyView);
    s.plank.forEach(updateBodyView);
    s.crocodile.forEach(updateBodyView);
    //Use updateBodyView function for fly
    updateBodyView(s.fly)

    //if frog lands on the target box, unhide the frog placed on target box in HTML (filling up the box)
    //if condition check that utilisedTargetBox is not null
    if (s.utilisedTargetBox) {
      const v = document.getElementById(String(s.utilisedTargetBox.id))!;
      if (v) v.classList.remove('hidden')
    }

    //If fly is eaten, hide the fly
    if (s.flyEaten) {
      const v = document.getElementById(String(s.fly.id))!;
      if (v) v.classList.add('hidden')
    }

    //Show instruction on how to restart if gameover
    if(s.gameOver) {
      //Append text to show game over and instruction
      const v = document.createElementNS(svg.namespaceURI, "text")!;
      attr(v,{x:230,y:Constants.CanvasSizeY/2.5,class:"gameover",id:"gameover"});
      v.textContent = "Game Over";
      svg.appendChild(v);
      const z = document.createElementNS(svg.namespaceURI, "text")!;
      attr(z,{x:230,y:Constants.CanvasSizeY/2.1,class:"gameover_instruction",id:"gameover_instruction"});
      z.textContent = "Press spacebar to restart";
      svg.appendChild(z);
      //re-hide the target box
      initialState.targetBox.forEach(b=>document.getElementById(String(b.id))!.classList.add('hidden'))
      //Unhide the fly
      document.getElementById(String(initialState.fly.id))!.classList.remove('hidden')
    }

    //Remove instruction for gameover after user restart the game
    if(!s.gameOver) {
      const v = document.getElementById("gameover")
      const z = document.getElementById("gameover_instruction")
      if (v) svg.removeChild(v);
      if (z) svg.removeChild(z);
    }

    //If user win a certain level and level up
    if(s.win) {
      //re-hide the target box
      initialState.targetBox.forEach(b=>document.getElementById(String(b.id))!.classList.add('hidden'))
      //unhide the fly
      document.getElementById(String(initialState.fly.id))!.classList.remove('hidden')
    }

    //Update the score
    const score = document.getElementById("round_score")!
    score.textContent = String(s.score);

    //Update the highest score 
    const highest_score = document.getElementById("highest_score")!
    highest_score.textContent = String(s.highestScore);

    //Update the lives count
    const lives = document.getElementById("lives")!
    lives.textContent = String(s.lives);

    //Update the level
    const level = document.getElementById("level")!
    level.textContent = String(s.level);

    //Update the frog position
    const frog = document.getElementById("frog")!;
    frog.setAttribute('transform',
     `translate(${s.frog.pos.x},${s.frog.pos.y}) rotate(${s.frog.rotate},${s.frog.width/2},${s.frog.height/2})` )
     frog.setAttribute('transform-origin', '0,center')
    svg.appendChild(frog) //so frog is on top of plank 
  }

  const
    attr = (e:Element, o:{ [key:string]: Object }) =>
      { for(const k in o) e.setAttribute(k,String(o[k])) }

  ///////////////////////////////////////////////Main game stream///////////////////////////////////////////////////
  //merge will merge multiple stream of observable into single stream
  //reduceState accept instances RightMove|LeftMove|UpMove|DownMove|Tick and state as arguments, 
  //returns an Observable that emits state (since reduceState function returns state) 
  //subscribe to the Obserable (state) by passing each state into updateView function
  const subscription = merge(gameClock,rightMove,leftMove,upMove,downMove,restart).pipe(
    scan(reduceState, initialState)).subscribe(updateView)
}



// The following simply runs your main function on window load.  Make sure to leave it in place.
if (typeof window !== "undefined") {
  window.onload = () => {
    main();
  };
}
