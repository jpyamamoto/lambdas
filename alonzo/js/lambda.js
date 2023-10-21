import '/js/prism'

Prism.languages.lambda = {
  typeoutput: {
    pattern: /Type \[Line .+\]:/,
    greedy: true,
  },
  output: {
    pattern: /Output \[Line .+\]:/,
    greedy: true,
  },
  comment: {
    pattern: /(--.*)|({--.*--})/,
    greedy: true,
  },
  command: {
    pattern: /:(e|i|t)/,
    greddy: true,
  },
  punctuation: [
    /(?:['`,]?\(|[)\[\]])/,
    {
      pattern: /\.|->|::|:|=|≐/,
      lookbehind: true
    },
  ],
  boolean: /\b(?:false|true)\b/,
  number: /-?\b\d+(?:\.\d+)?(?:e[+-]?\d+)?\b/i,
  symbol: {
    pattern: /\b(?<!:)[a-z]+\b/,
    greedy: true,
  },
  type: {
    pattern: /\b(?<!:)[A-Z]\w+\b/,
    greedy: true,
  },
  lambdas: {
    pattern: /λ|Λ|∀/,
    greddy: true,
  },
};

Prism.languages.webmanifest = Prism.languages.lambda;
