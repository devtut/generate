---
metaTitle: ".NET Framework - SpeechRecognitionEngine class to recognize speech"
description: "Asynchronously recognizing speech based on a restricted set of phrases, Asynchronously recognizing speech for free text dictation"
---

# SpeechRecognitionEngine class to recognize speech

## Asynchronously recognizing speech based on a restricted set of phrases

```dotnet
SpeechRecognitionEngine recognitionEngine = new SpeechRecognitionEngine();
GrammarBuilder builder = new GrammarBuilder();
builder.Append(new Choices("I am", "You are", "He is", "She is", "We are", "They are"));
builder.Append(new Choices("friendly", "unfriendly"));
recognitionEngine.LoadGrammar(new Grammar(builder));
recognitionEngine.SpeechRecognized += delegate(object sender, SpeechRecognizedEventArgs e)
{
    Console.WriteLine("You said: {0}", e.Result.Text);
};
recognitionEngine.SetInputToDefaultAudioDevice();
recognitionEngine.RecognizeAsync(RecognizeMode.Multiple);

```

## Asynchronously recognizing speech for free text dictation

```dotnet
using System.Speech.Recognition;

// ...

SpeechRecognitionEngine recognitionEngine = new SpeechRecognitionEngine();
recognitionEngine.LoadGrammar(new DictationGrammar());
recognitionEngine.SpeechRecognized += delegate(object sender, SpeechRecognizedEventArgs e)
{
    Console.WriteLine("You said: {0}", e.Result.Text);
};
recognitionEngine.SetInputToDefaultAudioDevice();
recognitionEngine.RecognizeAsync(RecognizeMode.Multiple);

```

#### Syntax

- SpeechRecognitionEngine()
- SpeechRecognitionEngine.LoadGrammar(Grammar grammar)
- SpeechRecognitionEngine.SetInputToDefaultAudioDevice()
- SpeechRecognitionEngine.RecognizeAsync(RecognizeMode mode)
- GrammarBuilder()
- GrammarBuilder.Append(Choices choices)
- Choices(params string[] choices)
- Grammar(GrammarBuilder builder)

#### Parameters

| `LoadGrammar`: Parameters               | Details                                                                                                                                                   |
| --------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| grammar                                 | The grammar to load. For example, a `DictationGrammar` object to allow free text dictation.                                                               |
| **`RecognizeAsync`: Parameters**        | **Details**                                                                                                                                               |
| mode                                    | The `RecognizeMode` for the current recognition: `Single` for just one recognition, `Multiple` to allow multiple.                                         |
| **`GrammarBuilder.Append`: Parameters** | **Details**                                                                                                                                               |
| choices                                 | Appends some choices to the grammar builder. This means that, when the user inputs speech, the recognizer can follow different "branches" from a grammar. |
| **`Choices` constructor: Parameters**   | **Details**                                                                                                                                               |
| choices                                 | An array of choices for the grammar builder. See `GrammarBuilder.Append`.                                                                                 |
| **`Grammar` constructor: Parameter**    | **Details**                                                                                                                                               |
| builder                                 | The `GrammarBuilder` to construct a `Grammar` from.                                                                                                       |

#### Remarks

To use `SpeechRecognitionEngine`, your Windows version needs to have speech recognition enabled.

You have to add a reference to `System.Speech.dll` before you can use the speech classes.
