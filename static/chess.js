class ChessGame {
  constructor() {
    this.gameId = null;
    this.board = null;
    this.currentPosition = null;
    this.selectedSquare = null;
    this.legalMoves = [];
    this.moveHistory = [];
    this.capturedPieces = { white: [], black: [] };
    this.promotionMove = null;
    
    this.initElements();
    this.initEvents();
    this.createNewGame();
  }
  
  initElements() {
    this.boardElement = document.getElementById('chess-board');
    this.statusElement = document.getElementById('status');
    this.movesListElement = document.getElementById('moves-list');
    this.capturedWhiteElement = document.getElementById('captured-white');
    this.capturedBlackElement = document.getElementById('captured-black');
    this.promotionPopupElement = document.getElementById('promotion-popup');
    this.newGameButton = document.getElementById('new-game-btn');
  }
  
  initEvents() {
    this.newGameButton.addEventListener('click', () => this.createNewGame());
    
    // Handle promotion piece selection
    const promotionPieces = document.querySelectorAll('.promotion-piece');
    promotionPieces.forEach(piece => {
      piece.addEventListener('click', (e) => {
        const pieceType = e.target.getAttribute('data-piece');
        this.completePromotion(pieceType);
      });
    });
  }
  
  async createNewGame() {
    try {
      const response = await fetch('/api/new');
      const data = await response.json();
      this.gameId = data.game_id;
      this.updatePosition(data.position);
      this.moveHistory = [];
      this.capturedPieces = { white: [], black: [] };
      this.updateCapturedPieces();
      this.updateMovesList();
    } catch (error) {
      console.error('Error creating new game:', error);
      this.setStatus('Error creating new game. Please try again.');
    }
  }
  
  async loadGame(gameId) {
    try {
      const response = await fetch(`/api/game?id=${gameId}`);
      const data = await response.json();
      this.gameId = data.game_id;
      this.updatePosition(data.position);
    } catch (error) {
      console.error('Error loading game:', error);
      this.setStatus('Error loading game. Please try again.');
    }
  }
  
  async getLegalMoves() {
    try {
      const response = await fetch(`/api/moves?id=${this.gameId}`);
      const data = await response.json();
      this.legalMoves = data.moves;
      return this.legalMoves;
    } catch (error) {
      console.error('Error getting legal moves:', error);
      this.setStatus('Error getting legal moves.');
      return [];
    }
  }
  
  async makeMove(move) {
    try {
      const response = await fetch(`/api/move?id=${this.gameId}`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(move),
      });
      
      const data = await response.json();
      
      if (data.legal) {
        const oldPosition = this.currentPosition;
        this.updatePosition(data.position);
        
        // Update move history
        this.moveHistory.push({
          move: data.move,
          position: data.position
        });
        
        // Update captured pieces
        this.updateCapturedPiecesFromMove(oldPosition, data.position);
        
        // Update UI
        this.updateMovesList();
        this.updateCapturedPieces();
      } else {
        console.error('Illegal move:', move);
        this.setStatus('Illegal move. Please try again.');
      }
      
      return data.legal;
    } catch (error) {
      console.error('Error making move:', error);
      this.setStatus('Error making move. Please try again.');
      return false;
    }
  }
  
  updatePosition(position) {
    this.currentPosition = position;
    this.renderBoard();
    this.updateStatus();
    this.getLegalMoves();
  }
  
  renderBoard() {
    // Clear the board
    this.boardElement.innerHTML = '';
    
    // Create squares and pieces
    for (let row = 0; row < 8; row++) {
      for (let col = 0; col < 8; col++) {
        const square = document.createElement('div');
        square.className = `square ${(row + col) % 2 === 0 ? 'light' : 'dark'}`;
        square.setAttribute('data-row', row);
        square.setAttribute('data-col', col);
        
        // Add coordinates labels
        if (col === 0) {
          const rankLabel = document.createElement('div');
          rankLabel.className = 'rank-label';
          rankLabel.textContent = 8 - row;
          square.appendChild(rankLabel);
        }
        
        if (row === 7) {
          const fileLabel = document.createElement('div');
          fileLabel.className = 'file-label';
          fileLabel.textContent = String.fromCharCode(97 + col);
          square.appendChild(fileLabel);
        }
        
        // Add piece if any
        if (this.currentPosition.board[row][col]) {
          const piece = this.currentPosition.board[row][col];
          square.textContent = this.getPieceSymbol(piece);
        }
        
        // Add event listener
        square.addEventListener('click', (e) => this.handleSquareClick(row, col));
        
        this.boardElement.appendChild(square);
      }
    }
    
    // Mark king in check
    if (this.currentPosition.check) {
      const kingColor = this.currentPosition.turn;
      for (let row = 0; row < 8; row++) {
        for (let col = 0; col < 8; col++) {
          const piece = this.currentPosition.board[row][col];
          if (piece && piece.piece_type === "king" && piece.color === kingColor) {
            const square = this.boardElement.querySelector(`[data-row="${row}"][data-col="${col}"]`);
            square.classList.add('check');
          }
        }
      }
    }
  }
  
  getPieceSymbol(piece) {
    const symbols = {
      'white': {
        'pawn': '♙',
        'knight': '♘',
        'bishop': '♗',
        'rook': '♖',
        'queen': '♕',
        'king': '♔'
      },
      'black': {
        'pawn': '♟',
        'knight': '♞',
        'bishop': '♝',
        'rook': '♜',
        'queen': '♛',
        'king': '♚'
      }
    };
    return symbols[piece.color][piece.piece_type];
  }
  
  handleSquareClick(row, col) {
    if (!this.gameId || this.currentPosition.checkmate || this.currentPosition.stalemate) {
      return;
    }
    
    // If we're waiting for promotion, ignore other clicks
    if (this.promotionMove) {
      return;
    }
    
    const clickedPiece = this.currentPosition.board[row][col];
    
    // If no square is selected, select this one if it has a piece of the current player's color
    if (!this.selectedSquare) {
      if (clickedPiece && clickedPiece.color === this.currentPosition.turn) {
        this.selectedSquare = { row, col };
        this.highlightLegalMoves(row, col);
      }
      return;
    }
    
    // If the same square is clicked again, deselect it
    if (this.selectedSquare.row === row && this.selectedSquare.col === col) {
      this.clearHighlights();
      this.selectedSquare = null;
      return;
    }
    
    // If another square with the current player's piece is clicked, select that instead
    if (clickedPiece && clickedPiece.color === this.currentPosition.turn) {
      this.clearHighlights();
      this.selectedSquare = { row, col };
      this.highlightLegalMoves(row, col);
      return;
    }
    
    // Check if the move is legal
    const move = this.findLegalMove(this.selectedSquare.row, this.selectedSquare.col, row, col);
    if (move) {
      // Check if this is a pawn promotion move
      const piece = this.currentPosition.board[this.selectedSquare.row][this.selectedSquare.col];
      const isPromotion = piece && 
                          piece.piece_type === 'pawn' && 
                          ((piece.color === 'white' && row === 0) || 
                           (piece.color === 'black' && row === 7));
      
      if (isPromotion) {
        // Show promotion popup
        const targetSquare = this.boardElement.querySelector(`[data-row="${row}"][data-col="${col}"]`);
        const rect = targetSquare.getBoundingClientRect();
        this.showPromotionPopup(rect, { from: move.from, to: move.to });
      } else {
        this.makeMove({ from: move.from, to: move.to });
        this.clearHighlights();
        this.selectedSquare = null;
      }
    }
  }
  
  findLegalMove(fromRow, fromCol, toRow, toCol) {
    const fromCoord = this.coordsToAlgebraic(fromRow, fromCol);
    const toCoord = this.coordsToAlgebraic(toRow, toCol);
    
    return this.legalMoves.find(move => 
      move.from === fromCoord && move.to === toCoord
    );
  }
  
  coordsToAlgebraic(row, col) {
    const file = String.fromCharCode(97 + col);
    const rank = 8 - row;
    return `${file}${rank}`;
  }

  algebraicToCoords(algebraic) {
    const col = algebraic.charCodeAt(0) - 97;
    const row = 8 - parseInt(algebraic[1]);
    return [row, col];
  }

  highlightLegalMoves(row, col) {
    const fromCoord = this.coordsToAlgebraic(row, col);
    const legalTargets = this.legalMoves
      .filter(move => move.from === fromCoord)
      .map(move => this.algebraicToCoords(move.to));

    legalTargets.forEach(([targetRow, targetCol]) => {
      const square = this.boardElement.querySelector(`[data-row="${targetRow}"][data-col="${targetCol}"]`);
      square.classList.add('move-target');
      if (this.currentPosition.board[targetRow][targetCol]) {
        square.classList.add('capture');
      }
    });

    const selectedSquare = this.boardElement.querySelector(`[data-row="${row}"][data-col="${col}"]`);
    selectedSquare.classList.add('selected');
  }

  clearHighlights() {
    const squares = this.boardElement.querySelectorAll('.square');
    squares.forEach(square => {
      square.classList.remove('selected', 'move-target', 'capture');
    });
  }

  showPromotionPopup(targetRect, move) {
    this.promotionMove = move;
    const popup = this.promotionPopupElement;
    popup.style.display = 'block';
    // Remove the position calculation since CSS will handle centering
    popup.style.left = '';
    popup.style.top = '';
  }

  async completePromotion(pieceType) {
    if (!this.promotionMove) return;

    const move = {
      ...this.promotionMove,
      promotion: pieceType
    };

    this.promotionPopupElement.style.display = 'none';
    await this.makeMove(move);
    this.clearHighlights();
    this.selectedSquare = null;
    this.promotionMove = null;
  }

  updateStatus() {
    let status = '';
    if (this.currentPosition.checkmate) {
      const winner = this.currentPosition.turn === 'white' ? 'Black' : 'White';
      status = `Checkmate! ${winner} wins!`;
    } else if (this.currentPosition.stalemate) {
      status = 'Stalemate! Game is drawn.';
    } else {
      const turn = this.currentPosition.turn === 'white' ? 'White' : 'Black';
      status = `${turn} to move`;
      if (this.currentPosition.check) {
        status += ' (Check!)';
      }
    }
    this.statusElement.textContent = status;
  }

  updateMovesList() {
    this.movesListElement.innerHTML = '';
    for (let i = 0; i < this.moveHistory.length; i += 2) {
      const moveRow = document.createElement('div');
      moveRow.className = 'move-row';

      const moveNumber = document.createElement('span');
      moveNumber.className = 'move-number';
      moveNumber.textContent = `${Math.floor(i/2 + 1)}.`;

      const whiteMove = document.createElement('span');
      whiteMove.className = 'move';
      whiteMove.textContent = this.moveHistory[i].move.to;

      const blackMove = document.createElement('span');
      blackMove.className = 'move';
      if (this.moveHistory[i + 1]) {
        blackMove.textContent = this.moveHistory[i + 1].move.to;
      }

      moveRow.appendChild(moveNumber);
      moveRow.appendChild(whiteMove);
      moveRow.appendChild(blackMove);
      this.movesListElement.appendChild(moveRow);
    }
    this.movesListElement.scrollTop = this.movesListElement.scrollHeight;
  }

  updateCapturedPiecesFromMove(oldPosition, newPosition) {
    // Find only the newly captured piece
    for (let row = 0; row < 8; row++) {
      for (let col = 0; col < 8; col++) {
        const oldPiece = oldPosition.board[row][col];
        const newPiece = newPosition.board[row][col];
        if (oldPiece && newPiece && oldPiece.color !== newPiece.color) {
            this.capturedPieces[oldPiece.color].push(oldPiece);
            return;
        }
      }
    }
  }

  updateCapturedPieces() {
    this.capturedWhiteElement.innerHTML = '';
    this.capturedBlackElement.innerHTML = '';

    this.capturedPieces.white.forEach(piece => {
      const pieceElement = document.createElement('span');
      pieceElement.textContent = this.getPieceSymbol(piece);
      this.capturedWhiteElement.appendChild(pieceElement);
    });

    this.capturedPieces.black.forEach(piece => {
      const pieceElement = document.createElement('span');
      pieceElement.textContent = this.getPieceSymbol(piece);
      this.capturedBlackElement.appendChild(pieceElement);
    });
  }
}

// Initialize the game when the page loads
document.addEventListener('DOMContentLoaded', () => {
  new ChessGame();
});