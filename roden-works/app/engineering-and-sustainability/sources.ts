import type { Source } from '@/components/ui/Sources'
import { ENFRA_SOURCES } from './enfra/sources'
import { CONVERGINT_SOURCES } from './convergint/sources'

const pick = (list: Source[], id: string): Source => {
  const s = list.find((x) => x.id === id)
  if (!s) throw new Error(`Unknown source id "${id}"`)
  return s
}

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
// Entries are shared with the case-study pages so titles and URLs stay in one place.
export const ENGINEERING_SOURCES: Source[] = [
  pick(ENFRA_SOURCES, 'rrh-announcement'),
  pick(CONVERGINT_SOURCES, 'daily-herald-hq'),
  pick(CONVERGINT_SOURCES, 'ssn-schaumburg'),
]
