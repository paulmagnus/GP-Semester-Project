package com.zetcode;

public class GameState {
    private Board board;

    public int[] getPlayerPosition();

    public bool PlayerShotExist();

    public int[] getShotPositions();
    
    public int[][] getAlienPositions();

    public int[][] getBombPositions();
}