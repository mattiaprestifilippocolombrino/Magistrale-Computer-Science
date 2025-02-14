/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package matchingpair;

/**
 *
 * @author matti
 */
/*
Stati possibili che può avere una carta
*/
public enum CardState {
    EXCLUDED,   //Stato assunto in presenza di match, colore rosso 
    FACE_DOWN,  //Stato assunto di default, colore verde  
    FACE_UP    ////Stato assunto in quando una carta viene cliccata
}

