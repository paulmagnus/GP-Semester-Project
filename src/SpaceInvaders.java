package push307;

import java.awt.EventQueue;
import javax.swing.JFrame;

import clojure.lang.PersistentList;

public class SpaceInvaders extends JFrame implements Commons {

    private static final long serialVersionUID = 1L;

    private Board board;

    public SpaceInvaders(PersistentList pushProgram, long seed) {

        initUI(pushProgram, seed);
    }

    private void initUI(PersistentList pushProgram, long seed) {
        board = new Board(pushProgram, seed);

        add(board);
        setTitle("Space Invaders");
        // uncomment when running without clojure
        // setDefaultCloseOperation(EXIT_ON_CLOSE);
        // setVisible(true);

        // board.run();

        setSize(BOARD_WIDTH, BOARD_HEIGHT);
        setLocationRelativeTo(null);
        setResizable(false);
    }

    public int[] getResult() {
        board.run();
        return board.getScore();
    }

    // only use this for running without clojure
    // public static void main(String[] args) {
        
    //     EventQueue.invokeLater(() -> {
    //         SpaceInvaders ex = new SpaceInvaders();
    //         ex.setVisible(true);
    //     });
    // }

    // public static int runMe(PersistentList pushProgram) {
    //     EventQueue.invokeLater(() -> {
    //             SpaceInvaders ex = new SpaceInvaders(pushProgram);
    //             ex.setVisible(true);
    //         });
    //     // return ex.getResult();
    //     return 4;
    // }
}



