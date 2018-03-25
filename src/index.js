import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import {formatter, formatIfValid} from './js/phoneNumber'

const app = Main.embed(document.getElementById('root'));

registerServiceWorker();

// javascript interop

app.ports.toPrettyPrinter.subscribe(function(countryCodeAndNumber) {
  app.ports.fromPrettyPrinter.send(formatter({countryCode: countryCodeAndNumber[0],
                                              number: countryCodeAndNumber[1]}))
})

app.ports.toFormatIfValid.subscribe(function(countryCodeAndNumber) {
  console.log({countryCodeAndNumber})
  app.ports.fromFormatIfValid.send(formatIfValid({countryCode: countryCodeAndNumber[0],
                                                  number: countryCodeAndNumber[1]}))
})
