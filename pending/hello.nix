let
  greet = name: {
    script = ''
      echo "Hello, ${name}!"
    '';
  };

in (greet "John").script
