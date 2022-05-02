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
      cant: 0,
      grid: null,
      complete: false,  // true if game is complete, false otherwise
      waiting: true,
      Xinicial: null,
      Yinicial: null,
      seleccionInicial: true,
      capturadas: null
    };
    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);

    this.Historial = new Array();
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

  iniciar(i,j) {
    if(this.state.seleccionInicial){
    this.setState({
      Xinicial: i,
      Yinicial: j,
      waiting: false,
      capturadas: "[["+i+","+j+"]]"
    });
  }
  }

  handleClick(color) {
    // No action on click if game is complete or we are waiting.
    if (this.state.complete || this.state.waiting) {
      return;
    }
    // Build Prolog query to apply the color flick.
    // The query will be like:
    // flick([Xinicial,Yinicial,[g,g,b,g,v,y,p,v,b,p,v,p,v,r],
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
    //        [v,g,p,b,v,v,g,g,g,b,v,g,g,g]],r, Grid)
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const capturadas = JSON.stringify(this.state.capturadas).replaceAll('"', "");
    let queryS;
    if(this.state.seleccionInicial){
      queryS = "flickInicial(" + gridS + "," + color + "," + capturadas + ",Grid,NewCapturadas,Cant)";
      this.setState({
        seleccionInicial: false
      });
    }
    else
      queryS = "flickGeneral(" + gridS + "," + color + "," + capturadas + ",Grid,NewCapturadas,Cant)";
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
        });
      } else {
        // Prolog query will fail when the clicked color coincides with that in the top left cell.
        this.setState({
          waiting: false
        });
      }
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
        </div>
        <Board 
        grid={this.state.grid} 
        onClick={(i,j) => this.iniciar(i,j)}
        esInicial={(i,j) => this.state.Xinicial===i && this.state.Yinicial===j}
        />
        <div className='rightPanel'>
          <div className='historialPanel'>
            <div className='historialTexto'>Historial de Jugadas</div>
            <div className='historialColores'>{this.Historial.map(color =>
              <button
                className='historialCuadrado'
                style={{backgroundColor: colorToCss(color)}}
              />)}</div>
          </div>
        </div>
      </div>
    );
  }
}

export default Game;