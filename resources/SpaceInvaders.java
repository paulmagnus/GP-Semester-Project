package com.zetcode;

import java.awt.EventQueue;
import javax.swing.JFrame;

public class SpaceInvaders extends JFrame implements Commons {

    public SpaceInvaders() {

        initUI();
    }

    private void initUI() {

        add(new Board());
        setTitle("Space Invaders");
        // uncomment when running without clojure
        // setDefaultCloseOperation(EXIT_ON_CLOSE);
        setSize(BOARD_WIDTH, BOARD_HEIGHT);
        setLocationRelativeTo(null);
        setResizable(false);
    }

    // only use this for running without clojure
    public static void main(String[] args) {
        
        EventQueue.invokeLater(() -> {
            SpaceInvaders ex = new SpaceInvaders();
            ex.setVisible(true);
        });
    }
}



