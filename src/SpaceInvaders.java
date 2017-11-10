package push307;

import java.awt.EventQueue;
import javax.swing.JFrame;

import clojure.lang.PersistentList;

public class SpaceInvaders extends JFrame implements Commons {

    private static final long serialVersionUID = 1L;

    public SpaceInvaders(PersistentList pushProgram) {

        initUI(pushProgram);
    }

    private void initUI(PersistentList pushProgram) {

        add(new Board(pushProgram));
        setTitle("Space Invaders");
        // uncomment when running without clojure
        // setDefaultCloseOperation(EXIT_ON_CLOSE);
        setSize(BOARD_WIDTH, BOARD_HEIGHT);
        setLocationRelativeTo(null);
        setResizable(false);
    }

    // only use this for running without clojure
    // public static void main(String[] args) {
        
    //     EventQueue.invokeLater(() -> {
    //         SpaceInvaders ex = new SpaceInvaders();
    //         ex.setVisible(true);
    //     });
    // }
}



