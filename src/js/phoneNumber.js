import {AsYouType, parse, format} from 'libphonenumber-js'
import metadata from '../phonemetadata.min.json'

function tryParse(number, countryCode) {
  const parseAttempt = parse(number, countryCode)
  if (parseAttempt.phone) {return parseAttempt}
  return false
}

function formatIfValid({number, countryCode}) {
  const parsed = tryParse(number, countryCode)
  if (parsed) {return format(parsed, 'E.164')}
  return null
}

function formatter({number, countryCode}) {
  return new AsYouType(countryCode, metadata).input(number)
}

export {formatter, formatIfValid}
