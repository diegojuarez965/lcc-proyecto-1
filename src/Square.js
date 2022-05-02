import React from 'react';
import { colorToCss } from './Game';

class Square extends React.Component {
    render() {
        if(this.props.esInicial()){
            return(
                <button 
                style={{
                borderWidth:3,
                borderColor:'rgba(0,0,0.8)',
                backgroundColor: colorToCss(this.props.value), 
                }}
                />
            )
        }
        return (
            <button 
            style={{ backgroundColor: colorToCss(this.props.value) }} 
            onClick={() => this.props.onClick()}
            />
        );
    }
}

export default Square;