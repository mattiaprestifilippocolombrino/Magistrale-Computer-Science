/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package matchingpair;

import javax.swing.JLabel;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

//Classe che estende JLabel, che conta quante volte l’utente ha girato una carta. Ascolta i cambiamenti di "state" delle Card e gli shuffle
public class Counter extends JLabel implements PropertyChangeListener, ShuffleListener {
    private int count = 0;  // memorizza quante volte una carta è stata girata FACE_UP.

    public Counter() {
        setText("Face-up count: 0");
    }

    
    // onShuffle viene chiamato quando arriva uno ShuffleEvent. Il Counter deve essere resettato a 0.
    @Override
    public void onShuffle(ShuffleEvent e) {
        count = 0; 
        setText("Face-up count: " + count);
    }

    //Metodo che ascolta i cambi di proprietà di "state" delle Card. Se la carta passa da FACE_DOWN a FACE_UP significa che il giocatore ha girato una carta. Incrementa quindi il counter.
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if ("state".equals(evt.getPropertyName())) {
            CardState oldState = (CardState) evt.getOldValue();
            CardState newState = (CardState) evt.getNewValue();            
            if (oldState == CardState.FACE_DOWN && newState == CardState.FACE_UP) {
                count++;
                setText("Face-up count: " + count);
            }
        }
    }
}
