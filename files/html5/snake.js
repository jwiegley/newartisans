// Game board is made up of 220 total square, 20 horizontal, 11 vertical.
// Food begins at 11,6
// Snake's initial length is 9 from (1,11) to (9,11)
// Each square is 15 x 15
function GameBoard(width, height) {

    // constants
    this.borderThickness = 3;
    this.boardHorizontalOffset = 10;
    this.boardVerticalOffset = 16;
    this.softKeyHeight = 22;
    this.sysTrayHeight = 21;
    this.squareWidth = 15;
    this.squareHeight = 15;

    this.width = width;
    this.height = height;

    this.GetDrawXOffset = function() {
        return this.boardHorizontalOffset;
    }

    this.GetDrawYOffset = function() {
        return this.boardVerticalOffset + this.sysTrayHeight;
    }

    this.IsValidPosition = function(x, y) {
        if (x >= 0 && x <= 19 && y >= 0 && y <= 10) {
            return true;
        }
        return false;
    }

    this.Draw = function(context) {
        context.fillStyle = "rgb(0,0,0)";
        context.fillRect(0, 0, this.width, this.height);

        // draw grey background
        context.fillStyle = "rgb(140,140,110)";
        context.fillRect(0, this.sysTrayHeight, this.width, this.height - this.sysTrayHeight - this.softKeyHeight);

        // draw green background
        context.fillStyle = "rgb(190,220,145)";
        context.fillRect(this.boardHorizontalOffset,
                         this.sysTrayHeight + this.boardVerticalOffset,
                         15 * 20,
                         15 * 11);
                         //this.width - this.boardHorizontalOffset * 2 - this.borderThickness * 2,
                         //this.height - this.sysTrayHeight - this.softKeyHeight - this.boardVerticalOffset * 2 - this.borderThickness * 2);

        // draw borders
        context.fillStyle = "rgb(0,32,0)";
        context.fillRect(this.boardHorizontalOffset - this.borderThickness,
                         this.sysTrayHeight + this.boardVerticalOffset - this.borderThickness,
                         this.width - this.boardHorizontalOffset * 2 + this.borderThickness * 2,
                         this.borderThickness);
        context.fillRect(this.boardHorizontalOffset - this.borderThickness,
                         this.height - this.softKeyHeight - this.boardVerticalOffset,
                         this.width - this.boardHorizontalOffset * 2 + this.borderThickness * 2,
                         this.borderThickness);
        context.fillRect(this.boardHorizontalOffset - this.borderThickness,
                         this.sysTrayHeight + this.boardVerticalOffset,
                         this.borderThickness,
                         this.height - this.boardVerticalOffset * 2 - this.sysTrayHeight - this.softKeyHeight + this.borderThickness);
        context.fillRect(this.width - this.boardHorizontalOffset,
                         this.sysTrayHeight + this.boardVerticalOffset,
                         this.borderThickness,
                         this.height - this.boardVerticalOffset * 2 - this.sysTrayHeight - this.softKeyHeight + this.borderThickness);
    }

}

function Food(x, y, width, height, left, top) {

    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
    this.left = left;
    this.top = top;

    this.Draw = function(context) {
        context.fillStyle = "rgb(140,140,110)";
        // TODO: draw a diamond shaped thing inside this given area
        context.fillRect(this.x * this.width + this.left, this.y * this.height + this.top, this.width, this.height);
    }
}

function Square(x, y, width, height, left, top) {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
    this.left = left;
    this.top = top;

    this.Draw = function(context, style) {
        context.fillStyle = style;
        context.fillRect(x * width + left, y * height + top, width, height);
    }
}

function Snake(segments) {

    this.segments = new Array();
    // 1 - left
    // 2 - up
    // 3 - right
    // 4 - down
    this.direction = 3;
    this.uncommitedDirection = new Array();

    this.died = false;

    for (i = 0; i < segments.length; i++) {
        this.segments.push(new Square(segments[i].x, segments[i].y, segments[i].width, segments[i].height, segments[i].left, segments[i].top));
    }

    this.Draw = function(context) {
        var style = "rgb(0,32,0)";
        /*
        if (this.died) {
            style = "rgb(140,140,110)";
        }
        */
        for (i = 0; i < this.segments.length; i++) {
            this.segments[i].Draw(context, style);
        }
    }

    this.Update = function(direction) {
        if (this.segments.length == 0) {
            return;
        }

        if (direction) {
            if ((this.direction == 1 && direction == 3) ||
                (this.direction == 2 && direction == 4) ||
                (this.direction == 3 && direction == 1) ||
                (this.direction == 4 && direction == 2)) {
                if (this.uncommitedDirection.length == 0) {
                    return;
                }
            }
            this.uncommitedDirection.push(direction);
            return;
        }

        if (this.uncommitedDirection.length != 0) {
            this.direction = this.uncommitedDirection.shift();
        }

        var front = this.segments[0];

        var last = this.segments.pop();

        if (this.direction == 1) {
            last.x = front.x - 1;
            last.y = front.y;
        }
        else if (this.direction == 2) {
            last.x = front.x;
            last.y = front.y - 1;
        }
        else if (this.direction == 3) {
            last.x = front.x + 1;
            last.y = front.y;
        }
        else if (this.direction == 4) {
            last.x = front.x;
            last.y = front.y + 1;
        }

        if (this.Intersect(last.x, last.y)) {
            this.died = true;
        }
        else {
            this.segments.unshift(new Square(last.x, last.y, last.width, last.height, last.left, last.top));
        }
    }

    this.Intersect = function(x, y) {
        for (i = 0; i < this.segments.length; i++) {
            if (this.segments[i].x == x &&
                this.segments[i].y == y) {
                return true;
            }
        }
        return false;
    }

}

function Game(context) {

    // drawing context
    this.context = context;

    // game loop variables
    this.gameLoop = null;

    // game states
    this.board = null;
    this.drawInterval = 100;
    this.food = null;
    this.snake = null;

    this.score = 0;
    this.scoreIncrement = 10;

    this.stopped = true;

    this.Initialize = function() {
        this.board = new GameBoard(this.context.canvas.width, this.context.canvas.height);

        var left = this.board.GetDrawXOffset();
        var top = this.board.GetDrawYOffset();

        this.food = new Food(10, 5, this.board.squareWidth, this.board.squareHeight, left, top);

        var segments = new Array();

        for (i = 8; i >= 0; i--) {
            var segment = new Square(i, 10, this.board.squareWidth, this.board.squareHeight, left, top);
            segments.push(segment);
        }

        this.snake = new Snake(segments);

        this.Draw(this.context);

        this.BindKey();
    }

    this.BindKey = function() {
        $(document).bind("keydown", function(event) {
            if (event.keyCode == 39 && document.snakeGame.stopped) {
                document.snakeGame.Start();
            }
            else if (!document.snakeGame.stopped) {
                document.snakeGame.RunGameLoop(event);
            }
        });
    }

    this.UnbindKey = function() {
        $(document).unbind("keydown");
    }

    this.Start = function() {
        this.score = 0;
        this.stopped = false;
        this.UnbindKey();
        this.Initialize();
        this.gameLoop = setInterval(this.RunGameLoop, this.drawInterval);
    }

    this.RunGameLoop = function(event) {
        document.snakeGame.Update(event);
        document.snakeGame.Draw();
    }

    this.Stop = function() {
        this.stopped = true;
        clearInterval(this.gameLoop);
    }

    this.Update = function(event) {
        if (event) {
            if (event.keyCode == 37) {
                this.snake.Update(1);
            }
            else if (event.keyCode == 38) {
                this.snake.Update(2);
            }
            else if (event.keyCode == 39) {
                this.snake.Update(3);
            }
            else if (event.keyCode == 40) {
                this.snake.Update(4);
            }
        }
        else {
            this.snake.Update();
        }
        
        if (this.snake.died ||
            !this.board.IsValidPosition(this.snake.segments[0].x, this.snake.segments[0].y)) {
            if (!this.snake.died) {
                this.snake.segments.shift();
            }
            this.Draw();
            this.Stop();

            alert("Game Over. Your score was " + this.score + ".");

            return;
        }

        if (this.snake.segments[0].x == this.food.x &&
            this.snake.segments[0].y == this.food.y) {

            this.score += this.scoreIncrement;

            do {
                this.food.x = Math.floor(Math.random()*20);
                this.food.y = Math.floor(Math.random()*11);
            } while (this.snake.Intersect(this.food.x, this.food.y));

            var last = this.snake.segments[this.snake.segments.length - 1];
            this.snake.segments.push(new Square(last.x, last.y, last.width, last.height, last.left, last.top));
        }
    }

    this.Draw = function() {
        // draw game board
        this.board.Draw(this.context);

        // draw score
        this.context.fillStyle = "rgb(256,256,256)";
        this.context.font = "12pt Arial";
        this.context.fillText("Snake (" + this.score + ")", 25, 16);

        // draw food square
        this.food.Draw(context);

        // draw snake squares
        this.snake.Draw(context);
    }

}

$(function() {
    var canvas = document.getElementById("canvas");
    var context = canvas.getContext("2d");

    document.snakeGame = new Game(context);
    document.snakeGame.Initialize();
});
