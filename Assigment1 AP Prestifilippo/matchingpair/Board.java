/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package matchingpair;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyChangeListener;
import java.util.*;
import java.util.List;

/*
  Finestra principale del gioco, mostra 8 carte (2 righe, 4 colonne), dove vengono posti i pulsanti e i label definiti nelle altre classi.
 È responsabile di generare nuovi valori per le carte ogni volta che si fa shuffle, notificando tutti i componenti tramite ShuffleEvent e PropertyChangeEvent("shuffleValues"). Per gli shuffleEvent, implementa il ruolo di concreteSubject.
*/
public class Board extends JFrame {
    private Card[] cards = new Card[8];
    private Controller controller;
    private Counter counter;
    private JButton shuffleButton;
    private ExitButton exitButton;

    private int[] shuffleValues; // Ultima sequenza di valori assegnata alle carte
    private PropertyChangeSupport pcs = new PropertyChangeSupport(this);    
    private List<ShuffleListener> shuffleListeners = new ArrayList<>(); // Lista dei listener interessati allo ShuffleEvent 

    //Inizializza il pannello con i vari pulsanti
    public Board() {
        super("Matching Pairs Game"); 
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        shuffleButton = new JButton("Shuffle");
        exitButton = new ExitButton();
        controller = new Controller();
        counter = new Counter();
        JPanel topPanel = new JPanel(new FlowLayout());
        topPanel.add(shuffleButton);
        topPanel.add(exitButton);
        topPanel.add(controller);
        topPanel.add(counter);
        // Inizializza le Card
        JPanel cardsPanel = new JPanel(new GridLayout(2,4,5,5));
        for (int i = 0; i < 8; i++) {
            cards[i] = new Card(); // Crea una nuova carta
            cardsPanel.add(cards[i]); // Aggiunge la carta al pannello
        }

        add(topPanel, BorderLayout.NORTH);
        add(cardsPanel, BorderLayout.CENTER);

        // Inizializza i listener. Per ogni carta:
        // - addBeanPropertyChangeListener(controller): Controller ascolta i cambi di stato delle carte (PropertyChange)
        // - addBeanVetoableChangeListener(controller): Controller può vetoare i cambi di stato non validi (VetoableChange)
        // - addBeanPropertyChangeListener(counter): Counter ascolta i cambi di stato delle carte per incrementare il conteggio
        //Ogni carta si iscrive in ascolto:
        // - addShuffleListener(c): Le carte ascoltano ShuffleEvent dal Board per tornare face_down
        // - controller.addMatchedListener(c): Le carte ascoltano MatchedEvent dal Controller per reagire ai match (excluded o face_down)
        for (Card c : cards) {
            c.addBeanPropertyChangeListener(controller);
            c.addBeanVetoableChangeListener(controller);
            c.addBeanPropertyChangeListener(counter);
            addShuffleListener(c);
            controller.addMatchedListener(c);
        }

        // Il Counter ascolta ShuffleEvent per reset del conteggio
        addShuffleListener(counter);
        // PropertyChange su "shuffleValues"
        addPropertyChangeListener(controller);

        // Azione sul pulsante Shuffle: richiama shuffleAndDeal() per generare nuovi valori
        shuffleButton.addActionListener(e -> shuffleAndDeal());
        // Azione sul pulsante Exit: chiede conferma, se sì chiude il programma
        exitButton.addActionListener(e -> {
            int choice = JOptionPane.showConfirmDialog(this, "Are you sure you want to exit?");
            if (choice == JOptionPane.YES_OPTION) {
                System.exit(0);
            }
        });

        setSize(600, 400);
        setLocationRelativeTo(null);
        setVisible(true); 
        shuffleAndDeal(); // All'avvio fa subito uno shuffle per iniziare la partita
    }

    // Metodi per aggiungere/rimuovere PropertyChangeListener sul Board
    public void addPropertyChangeListener(PropertyChangeListener l) {
        pcs.addPropertyChangeListener(l);
    }
    public void removePropertyChangeListener(PropertyChangeListener l) {
        pcs.removePropertyChangeListener(l);
    }

    // Metodi per la gestione degli ShuffleListener
    public void addShuffleListener(ShuffleListener l) {
        shuffleListeners.add(l);
    }
    public void removeShuffleListener(ShuffleListener l) {
        shuffleListeners.remove(l);
    }

    // setShuffleValues: imposta la nuova sequenza di valori e notifica i listener del cambio di "shuffleValues".  Crea un ShuffleEvent e notifica tutti gli ShuffleListener (carte, counter, controller)
    public void setShuffleValues(int[] newValues) {
        int[] old = this.shuffleValues;
        this.shuffleValues = newValues;
        pcs.firePropertyChange("shuffleValues", old, newValues);
        ShuffleEvent se = new ShuffleEvent(this, newValues);
        for (ShuffleListener sl : shuffleListeners) {
            sl.onShuffle(se);
        }
        // Assegna i nuovi valori alle carte
        for (int i=0; i<cards.length; i++) {
            cards[i].setValue(newValues[i]);
        }
    }

    private void shuffleAndDeal() {
        // Genera 4 coppie di valori casuali e li assegna alle carte
        int[] vals = generateRandomPairs(4);
        setShuffleValues(vals);
    }

    //Crea una lista di valori randomici, presi a coppia, ma in ordine casuale
    private int[] generateRandomPairs(int pairCount) {
    int maxNumber = 100; // ad esempio, scegli da 1 a 100
    List<Integer> availableNumbers = new ArrayList<>();
    for (int i = 1; i <= maxNumber; i++) {
        availableNumbers.add(i);
    }
    Collections.shuffle(availableNumbers);
    List<Integer> chosenNumbers = availableNumbers.subList(0, pairCount);
    List<Integer> list = new ArrayList<>();
    for (Integer num : chosenNumbers) {
        list.add(num);
        list.add(num);
    }
    Collections.shuffle(list);
    return list.stream().mapToInt(x -> x).toArray();
}

    public static void main(String[] args) {
        // Avvia l'applicazione Swing su EDT
        SwingUtilities.invokeLater(() -> new Board());
    }
}
