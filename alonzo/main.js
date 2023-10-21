import 'normalize.css'
import 'terminal.css'
import './style.css'
import CodeFlask from 'codeflask'
import '/js/prism'
import '/js/lambda'

const UNTYPED_CODE = `{-- Booleans --}

true  ≐ λ t . λ f . t
false ≐ λ t . λ f . f

-- Operators
not = λ b . b false true
and = λ a . λ b . a b false
or  = λ a . λ b . a true b
impl  = λ a . λ b . or (not a) b
equiv = λ a . λ b . and (impl a b) (impl b a)

-- Control
eq ≐ λ x . λ y . if (equiv x y) true false
if ≐ λ b . λ t . λ e . b t e

-- <=> Truth Table
:e equiv true  true
:e equiv true  false
:e equiv false true
:e equiv false false

-- Example If
:e if true x y
:e if false u v`;

const TYPED_CODE = `{-- Boolean Definitions --}
and = λ a : Bool . λ b : Bool . if a then (if b then true else false) else false
{-- End Boolean Definitions --}

-- Operations
pred = λ n : Nat . n - 1

-- Comparison
leq = λ x : Nat . λ y : Nat . iszero (x - y)
eqn = λ x : Nat . λ y : Nat . and (leq x y) (leq y x)

-- Complex Functions
factfunc = λ f : Nat -> Nat .
             λ n : Nat . if (eqn n 0)
                         then 1
                         else (n * (f (n - 1)))

factorial = fix factfunc

fibfunc = λ f : Nat -> Nat .
            λ n : Nat . if (eqn n 0)
                        then 0
                        else (if (eqn n 1)
                                 then 1
                                 else ((f (n - 1)) + (f (n - 2))))

fibonacci = fix fibfunc

isevenFunc = λ f : Nat -> Bool .
             λ x : Nat . if (iszero x)
                            then true
                            else if (iszero (pred x))
                                 then false
                                 else (f (pred (pred x)))

iseven = fix isevenFunc

-- Examples
:e 0 + 0
:e 0 + 1
:e 3 + 7
:e 7 + 3

:e pred 3
:e 3 - 1
:e 8 - 5

:e leq 10 10
:e eqn 10 10
:e eqn 10 9

:t iseven
:e iseven 8

:e factorial 0
:e factorial 1
:e factorial 3

:e fibonacci 0
:e fibonacci 1
:e fibonacci 7`;

const SYSTEMF_CODE = `-- Booleans

true  = Λ X . λ x : X . λ y : X . x
false = Λ X . λ x : X . λ y : X . y

:i true
:i false

:t true

Bool :: ∀ X . X -> X -> X

-- Operators
not = λ b : ∀ X . X -> X -> X . (b [Bool]) false true
and = λ a : Bool . λ b : Bool . (a [Bool]) b false
or = λ a : Bool . λ b : Bool . (a [Bool]) true b
impl  = λ a : Bool . λ b : Bool . or (not a) b
equiv = λ a : Bool . λ b : Bool . and (impl a b) (impl b a)

if = Λ X . λ b : Bool . λ t : X . λ f : X . b [X] t f

-- Control
eq ≐ λ x : Bool . λ y : Bool . equiv x y

-- <=> Truth Table
:e equiv true  true
:e equiv true  false
:e equiv false true
:e equiv false false

:e eq false false

:t if

-- Example If
:e if [Bool] true true false
:e if [Bool] false true false`;

const TOKENS = {
  '\\bforall ': '∀',
  '\\blambda ': 'λ',
  '\\\\ '     : 'λ',
  '\\bLambda ': 'Λ',
  '( |^)= '   : ' ≐',
  '( |^)= '   : ' ≐',
};

let reUpdate = true;

function replaceToken(token, replacement, code) {
  const re = new RegExp(token, 'g');
  return code.replaceAll(re, replacement + ' ');
}

const getCaretPosition = e => e && e.selectionStart || -1;

function setCaretPosition(elem, caretPos) {
  if(elem != null) {
    if(elem.createTextRange) {
      var range = elem.createTextRange();
      range.move('character', caretPos);
      range.select();
    } else {
      if(elem.selectionStart) {
        elem.focus();
        elem.setSelectionRange(caretPos, caretPos);
      } else
        elem.focus();
    }
  }
}


window.addEventListener('load', () => {
  const flask = new CodeFlask('#program-input', {
    language: 'lambda',
    defaultTheme: false,
    lineNumbers: true
  });

  const pre = document.getElementById('program-input');
  const textArea = pre.getElementsByTagName('textarea')[0];
  const selectedLang = document.getElementById('language');
  const outputSection = document.getElementById('output');
  const outputArea = outputSection.getElementsByTagName('code')[0];
  const exampleButton = document.getElementById('example');

  const INTERPRETERS = {
    'untyped': window.untyped,
    'typed': window.typed,
    'systemf': window.systemF,
  };

  document.getElementById('reset').addEventListener('click', () => {
    flask.updateCode('');
  });

  document.getElementById('run').addEventListener('click', (ev) => {
    ev.preventDefault();
    const code = flask.getCode();
    let output = INTERPRETERS[selectedLang.value](code);

    let error = false;

    if (output.startsWith('Parse Error')) {
      error = true;
    } else if (output.startsWith('Evaluation Error')) {
      error = true;
    } else if (output.startsWith('Typing Error')) {
      error = true;
    } else {
      error = false;
    }

    if (error) {
      outputSection.classList.add('terminal-alert-error');
    } else {
      outputSection.classList.remove('terminal-alert-error');
    }

    // For some reason the output of haskell is wrong
    output = output.replaceAll('픹', '𝔹');

    outputArea.textContent = output;
    Prism.highlightAllUnder(outputSection);
  });

  flask.onUpdate((code) => {
    if (!reUpdate) {
      reUpdate = true;
      return;
    }

    reUpdate = false;

    const currCaretPos = getCaretPosition(textArea);

    const leftCode = code.slice(0, currCaretPos);
    const rightCode = code.slice(currCaretPos);

    const newCode = Object.keys(TOKENS)
                          .reduce((currCode, key) =>
                            replaceToken(key, TOKENS[key], currCode), leftCode);

    const diff = (leftCode.length - newCode.length);
    const newPos = currCaretPos - diff;

    flask.updateCode(newCode + rightCode);
    setCaretPosition(textArea, newPos);
    window.Prism.highlightAllUnder(pre);
  });

  exampleButton.addEventListener('click', () => {
    const lang = selectedLang.value;

    let code = '';

    switch (lang) {
      case 'untyped':
        code = UNTYPED_CODE;
        break;
      case 'typed':
        code = TYPED_CODE;
        break;
      default:
        code = SYSTEMF_CODE;
    }

    flask.updateCode(code);
  });
});
