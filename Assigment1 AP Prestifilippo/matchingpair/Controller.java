/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package matchingpair;


import javax.swing.JLabel;
import java.beans.*;
import javax.swing.SwingUtilities;
import java.util.Timer;
import java.util.TimerTask;
import java.util.List;
import java.util.ArrayList;

/*Classe che estende JLabel, che contiene la logica di matching del gioco. Tiene traccia di quante coppie sono state trovate, incapsulando il valore in matchedCount.
  Funge da ConcreteSubject nel pattern Observer rispetto al MatchedEvent, mantenendo una lista di matchedListeners.
  Riceve eventi di cambiamento di stato dalle Card, con possibilità di porre veto. Implementa ShuffleListener, in modo da ricevere tale tipo di eventi.
*/
public class Controller extends JLabel implements PropertyChangeListener, VetoableChangeListener, ShuffleListener {
    private int matchedCount = 0;
    private boolean waitingFirstCard = true;    //Indica se si sta aspettando la prima carta del turno. Se true, la prossima carta face_up sarà la prima.
    private int firstCardValue;     //memorizza il valore della prima carta girata face_up nel turno corrente.

//    private PropertyChangeSupport pcs = new PropertyChangeSupport(this);
    private List<MatchedListener> matchedListeners = new ArrayList<>(); //lista di listener interessati agli eventi di match.

     // Imposta il testo iniziale del JLabel che mostra le coppie trovate.
    public Controller() {
        setText("Matched Pairs: 0");
    }

    // Viene chiamato quando arriva uno ShuffleEvent. Reset del matchedCount e della logica in modo da ricominciare la partita.
    @Override
    public void onShuffle(ShuffleEvent e) {
        matchedCount = 0;
        setText("Matched Pairs: " + matchedCount);
        waitingFirstCard = true;
    }

    //Metodo di aggiunta dei listener in ascolto per eventi di tipo Match
    public void addMatchedListener(MatchedListener l) {
        matchedListeners.add(l);
    }

    // Crea un nuovo MatchedEvent con il risultato del confronto (matched=true o false).Notifica tutti i MatchedListener interessati.
    private void fireMatchedEvent(boolean matched) {
        MatchedEvent evt = new MatchedEvent(this, matched);
        for (MatchedListener ml : matchedListeners) {
            ml.onMatched(evt);
        }
    }

        // Metodo che ascolta ascolta i cambi di proprietà generati da Bord per la property "shuffleValues" e Card per "state"
    @Override
    public void propertyChange(PropertyChangeEvent evt) {

            // Se l'evento proviene dal Board e la proprietà è "shuffleValues", crea uno ShuffleEvent a partire dal PropertyChangeEvent, e chiama il metodo di gestione onShuffle()
        if (evt.getSource() instanceof Board && evt.getPropertyName().equals("shuffleValues")) {
            ShuffleEvent se = new ShuffleEvent(evt.getSource(), (int[])evt.getNewValue());
            onShuffle(se);
        /*
        Se la proprietà cambiata è "state", significa che una carta ha cambiato stato. Qui di implementa la logica di matching.
        Se una carta è appena stata girata a FACE_UP, se stiamo aspettando la prima carta del turno, memorizziamo il suo valore e attendiamo la seconda carta.
        Se è la seconda carta girata FACE_UP nel turno, avviamo un Timer di 0.5s per permettere al giocatore di vedere la seconda carta prima di confrontare i valori.
        Se matched=true, incrementiamo matchedCount e emettiamo un MatchedEvent con l'esito del confronto. Anche in caso di non match, notifichiamo l'esito ai listener e resettiamo waitingFirstCard per il prossimo turno.
        */
        } else if (evt.getPropertyName().equals("state")) {
            Card c = (Card) evt.getSource();
            CardState newState = (CardState) evt.getNewValue();
            if (newState == CardState.FACE_UP) {
                int v = c.getValue(); 
                if (waitingFirstCard) {
                    firstCardValue = v;
                    waitingFirstCard = false;
                } else {
                    int secondValue = v;
                    Timer timer = new Timer();
                    timer.schedule(new TimerTask() {
                        @Override
                        public void run() {
                            SwingUtilities.invokeLater(() -> {
                                boolean matched = (firstCardValue == secondValue);
                                if (matched) {
                                    matchedCount++;
                                    setText("Matched Pairs: " + matchedCount);
                                }
                                fireMatchedEvent(matched);
                                waitingFirstCard = true;
                            });
                        }
                    }, 500);
                }
            }
        }
    }

    //Metodo chiamato prima di applicare un cambiamento sulla proprietà vincolata "state" delle Card.
    // Se la carta passa da FACE_UP o EXCLUDED a FACE_DOWN con un'azione utente (internalChange=false) il Controller deve vetoare il cambiamento, impedendo all'utente di girare la carta quando non consentito.
    @Override
    public void vetoableChange(PropertyChangeEvent evt) throws PropertyVetoException {
        if ("state".equals(evt.getPropertyName())) {
            Card c = (Card) evt.getSource();
            CardState oldState = (CardState) evt.getOldValue();
            CardState newState = (CardState) evt.getNewValue();
            if ((oldState == CardState.FACE_UP || oldState == CardState.EXCLUDED)
                && newState == CardState.FACE_DOWN 
                && !c.isInternalChange()) {
                throw new PropertyVetoException("Invalid state change by user click", evt);
            }
        }
    }
}
