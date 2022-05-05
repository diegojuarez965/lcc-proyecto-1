import React from 'react';
import Square from './Square';

class Board extends React.Component {
    render() {
        return (
            <div className="board">
                {this.props.grid.map((row, i) =>
                    row.map((cell, j) =>
                        <Square
                            value={cell}
                            key={i + "." + j}
                            onClick={() => this.props.onClick(i,j)} //Determina si una celda fue clickeada
                            esInicial={()=> this.props.esInicial(i,j)} //Determina si una celda es la inicial
                        />
                    )
                )}
            </div>
        );
    }
}

export default Board;