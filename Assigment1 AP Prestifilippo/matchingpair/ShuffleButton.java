/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package matchingpair;

import java.awt.event.ActionListener;
import javax.swing.JButton;

/**
 *
 * @author matti
 */
    //Pulsante per rimescolare le carte. Quando viene premuto , notifica il rimescolamento al Board.
public class ShuffleButton extends JButton {
    public ShuffleButton(ActionListener listener) {
        super("Shuffle"); // Imposta il testo del pulsante
        addActionListener(listener); // Aggiunge l'action listener passato come parametro
    }
}
