import tkinter as tk
from tkinter import messagebox
import subprocess

class TicTacToeGUI:
    def __init__(self, root):
        self.root = root
        self.root.title("tic_tac_toe")

        self.board = [0] * 9
        self.current_player = 1

        self.buttons = []
        for i in range(9):
            btn = tk.Button(root, text='', font=('Arial', 20), width=5, height=2,
                            command=lambda i=i: self.make_move(i))
            btn.grid(row=i // 3, column=i % 3)
            self.buttons.append(btn)

        self.init_game()

    def init_game(self):
        choice = messagebox.askquestion("Elección", "¿Quieres jugar como X?")
        if choice == 'yes':
            self.current_player = 1
        else:
            self.current_player = -1
            self.computer_move()

    def reset_game(self):
        self.board = [0] * 9
        for btn in self.buttons:
            btn.config(text='')
        self.init_game()

    def make_move(self, index):
        if self.board[index] == 0:
            self.board[index] = self.current_player
            self.update_ui()
            if not self.check_game_status():
                self.current_player *= -1
                self.computer_move()

    def computer_move(self):
        prolog_move = self.get_prolog_move()
        if prolog_move is not None:
            self.board[prolog_move] = self.current_player
            self.update_ui()
            if not self.check_game_status():
                self.current_player *= -1

    def get_prolog_move(self):
        prolog_input = ','.join(map(str, self.board))
        result = subprocess.run(['swipl', '-s', 'tic_tac_toe.pl', '-g', f'jugador_minimax([{prolog_input}], {self.current_player}, M), write(M), halt.'],
                                capture_output=True, text=True)
        try:
            return int(result.stdout.strip())
        except ValueError:
            return None

    def update_ui(self):
        for i in range(9):
            if self.board[i] == 1:
                self.buttons[i].config(text='X')
            elif self.board[i] == -1:
                self.buttons[i].config(text='O')
            else:
                self.buttons[i].config(text='')

    def check_game_status(self):
        winner = self.get_winner()
        if winner is not None:
            if winner == 1:
                messagebox.showinfo("Fin del juego", "¡X ganó!")
            elif winner == -1:
                messagebox.showinfo("Fin del juego", "¡O ganó!")
            else:
                messagebox.showinfo("Fin del juego", "¡Empate!")

            if messagebox.askyesno("Reiniciar", "¿Quieres jugar de nuevo?"):
                self.reset_game()
            else:
                self.root.quit()
            return True
        return False

    def get_winner(self):
        for line in [[0,1,2], [3,4,5], [6,7,8], [0,3,6], [1,4,7], [2,5,8], [0,4,8], [2,4,6]]:
            if self.board[line[0]] != 0 and self.board[line[0]] == self.board[line[1]] == self.board[line[2]]:
                return self.board[line[0]]
        if 0 not in self.board:
            return 0
        return None

if __name__ == "__main__":
    root = tk.Tk()
    app = TicTacToeGUI(root)
    root.mainloop()
