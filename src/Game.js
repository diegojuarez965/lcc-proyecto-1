import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

/**
 * List of colors.
 */

const colors = ["r", "v", "p", "g", "b", "y"];  // red, violet, pink, green, blue, yellow

/**
 * Returns the CSS representation of the received color.
 */

export function colorToCss(color) {
  switch (color) {
    case "r": return "red";
    case "v": return "violet";
    case "p": return "pink";
    case "g": return "green";
    case "b": return "blue";
    case "y": return "yellow";
  }
  return color;
}
class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.state = {
      turns: 0,
      cant: 0, //Es la cantidad de celdas capturadas
      grid: null,
      complete: false,  // true if game is complete, false otherwise
      waiting: true,
      Xinicial: null, //Es la coordenada X de la celda inicial
      Yinicial: null, //Es la coordenada Y de la celda inicial
      seleccionInicial: true, //Indica si se debe seleccionar la celda inicial
      capturadas: null, //Es el conjunto de celdas capturadas
      cantAyuda: 0, //Es la cantidad de celdas capturadas si se hace uso de la ayuda estratégica
    };
    this.handleClick = this.handleClick.bind(this);
    this.handleInput = this.handleInput.bind(this);
    this.iniciar = this.iniciar.bind(this);
    this.detectarFin = this.detectarFin.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);

    this.Historial = []; //Es el historial de jugadas o colores seleccionados
    this.SecuenciaAyuda = []; //Es la secuencia de jugadas de ayuda estratégica
  }


  handlePengineCreate() {
    const queryS = 'init(Grid)';
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid']
        });
      }
    });
  }


  /**
   * Inicializa las coordenadas de la celda inicial y forma el primer conjunto de celdas capturadas
   * @param {Es la coordenada X de la celda inicial} i 
   * @param {Es la coordenada Y de la celda inicial} j 
   */
  iniciar(i,j) {
    if(this.state.seleccionInicial){
      const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
      let queryS="obtenerCapturadasInicial(" + gridS + "," + "["+i+","+j+"]" + ",NewCapturadas,CantNewCapturadas,Complete)";
      this.setState({
        waiting: true
      });
      this.pengine.query(queryS, (success, response) => {
        if (success) {
          this.setState({
            Xinicial: i,
            Yinicial: j,
            waiting: false,
            cant: response['CantNewCapturadas'],
            capturadas: response['NewCapturadas'],
            complete: response['Complete']
          });
        }
        this.setState({
          waiting: false
        });
        this.detectarFin();
      })
    }
  }

  
  handleClick(color) {
    // No action on click if game is complete or we are waiting.
    if (this.state.complete || this.state.waiting) {
      return;
    }
    // Build Prolog query to apply the color flick.
    // The query will be like:
    // flick([[g,g,b,g,v,y,p,v,b,p,v,p,v,r],
    //        [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
    //        [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
    //        [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
    //        [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
    //        [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
    //        [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
    //        [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
    //        [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
    //        [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
    //        [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
    //        [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
    //        [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
    //        [v,g,p,b,v,v,g,g,g,b,v,g,g,g]], r, [[1,2],[1,1]], Grid, NewCapturadas, Cant, Complete)
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const capturadas = JSON.stringify(this.state.capturadas).replaceAll('"', "");
    let queryS="flick(" + gridS + "," + color + "," + capturadas + ",Grid,NewCapturadas,Cant,Complete)";
    this.setState({
      waiting: true
    });
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.Historial.push(color);
        this.setState({
          grid: response['Grid'],
          turns: this.state.turns + 1,
          waiting: false,
          cant: response['Cant'],
          capturadas: response['NewCapturadas'],
          seleccionInicial: false,
          complete: response['Complete']
        });
      }
      this.setState({
        waiting: false
      });
      this.detectarFin();
    });
  return;
  }


  /**
   * Detecta y ejecuta el fin del juego
   */
  detectarFin(){ 
    if(this.state.complete){
      setTimeout(function(){
        var seleccion = window.confirm("¡Fin del juego! Capturaste todas las celdas en "+this.state.turns+" turnos. ¿Desea volver a jugar?");
        if (seleccion === true) 
          window.location.reload();
        else{
          this.setState({
          complete: true
          });
          window.alert('¡Gracias por jugar!');
        }
    }.bind(this),1000);
    }
  }

  
  /** 
   * Maneja el evento de solicitud de ayuda estratégica 
   * @param {Es el tamaño máximo que tendrá la secuencia de jugadas de ayuda estratégica} numPE 
  */
  handleInput(numPE){
    if(numPE<=0 || this.state.complete || this.state.waiting) {
      return;
    }
    const greedy=1;
    const optimal=0;
    let metodo;
    if(numPE<=2) {       //Aca determinamos el metodo mas correcto a usar evitando que el metodo optimal crashee el programa
        if(this.state.cant<36)
           metodo=optimal;  //Parametros determinados a partir de algunos casos de pruebas realizados
        else
          metodo=greedy;
    } else if(numPE==3){
        if(this.state.cant<=7)
          metodo=optimal;
        else
          metodo=greedy;
    } else if(numPE==4) {
        if(this.state.cant<=3)
          metodo=optimal;
        else
          metodo=greedy;
    } else
        metodo=greedy;
       
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const capturadas = JSON.stringify(this.state.capturadas).replaceAll('"', "");
    const colores = JSON.stringify(["r", "v", "p", "g", "b", "y"]).replaceAll('"', "");
    let queryS = "ayudaEstrategia(" + numPE + "," + colores + "," + gridS + "," + capturadas +","+ metodo +",JugadasColores,CantFCapturadas)";
    let seleccion = this.state.seleccionInicial;
    if(seleccion)
      this.setState({
        seleccionInicial: false //Esto se hace para evitar seleccionar otras celdas si todavia no se selecciono ningun color y se esta esperando a que termine el metodo ayudaEstrategia
      });
    this.setState({
      waiting: true,
    });
    this.pengine.query(queryS, (success,response) => {
      if(success){
        this.SecuenciaAyuda = response['JugadasColores'];
        this.setState({
          cantAyuda: response['CantFCapturadas']
        });
      }
      this.setState({
        waiting: false,
        seleccionInicial: seleccion
      });
    });

  }


  render() {
    if (this.state.grid === null) {
      return null;
    }
    return (
      <div className="game">
        <div className="leftPanel">
          <div className="buttonsPanel">
            {colors.map(color =>
              <button
                className="colorBtn"
                style={{ backgroundColor: colorToCss(color) }}
                onClick={() => this.handleClick(color)}
                key={color}
              />)}
          </div>
          <div className="turnsPanel">
            <div className="turnsLab">Turnos</div>
            <div className="turnsNum">{this.state.turns}</div>
          </div>
          <div className='cantPanel'>
            <div className='cantLab'>Celdas Capturadas</div>
            <div className='cantNum'>{this.state.cant}</div>
          </div>
          <div className='ayudasPanel'>
              <div className='profundidadLab'>Profundidad Estrategia</div>
              <div className='profundidadInput'>
                <input type="number" defaultValue="1" name="inputProfunidad" id="inputProfundidad" max="100" min="1" step="1"></input>
              </div>
              <div className='botonAyudaEstrategia'>
                <input type="submit" value="Ayuda Estrategia" onClick={() => this.handleInput(document.getElementById("inputProfundidad").value)}></input>
              </div>
              <div className='resultadoAyudaEstrategia'>
                <div className='resultadoAyudaEstrategiaCantLab'>Cantidad de Capturadas:</div>
                <div className='resultadoAyudaEstrategiaCant'>{this.state.cantAyuda}</div>
                <div className='resultadoAyudaEstrategiaColoresLab'>Secuencia de Colores a realizar:</div>
                <div className='resultadoAyudaEstrategiaColores'>{this.SecuenciaAyuda.map(color =>
                  <button
                    className='resultadoAyudaEstrategiaCuadrado'
                    style={{backgroundColor: colorToCss(color)}}
                  />)}
                </div>
            </div>
          </div> 
        </div>
        <Board 
          grid={this.state.grid} 
          onClick={(i,j) => this.iniciar(i,j)} //Al clickear una celda del tablero llama al metodo iniciar
          esInicial={(i,j) => this.state.Xinicial===i && this.state.Yinicial===j} //Verifica si una celda es la celda inicial
        />
        <div className='rightPanel'>
          <div className='historialPanel'>
            <div className='historialTexto'>Historial de Jugadas</div>
            <div className='historialColores'>{this.Historial.map(color =>
              <button
                className='historialCuadrado'
                style={{backgroundColor: colorToCss(color)}} 
              />)}
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default Game;