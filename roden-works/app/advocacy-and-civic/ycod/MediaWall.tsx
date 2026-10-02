'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import { Cite } from '@/components/ui/Sources'
import { YCOD_SOURCES as S } from './sources'

// Verified coverage only (fact-check 2026-10). WKBW story syndicated across Scripps stations:
// https://www.wxyz.com/news/national/college-freshmen-in-new-york-develop-plan-to-encourage-more-organ-donors
// Spectrum News: https://spectrumlocalnews.com/nys/buffalo/news/2021/01/13/college-students-push-for-more-organ-donations-in-ny-
// WENY: https://weny.com/story/43131791/college-activists-pushing-for-change-to-organ-donor-registration-process-in-nys
// CBC, Yahoo News and Business Insider could not be found and were removed; "Local Media Network" was a placeholder.
// CBC Radio's Information Morning (Nova Scotia, host Portia Clark), 2020: owner-confirmed, Oct 2026;
// no archived link found, so no <Cite>. Radio One / CBC syndication items, Yahoo and Business Insider stay out.
const media: { name: string; type: string; sources?: string[] }[] = [
  { name: 'WKBW', type: 'Broadcast', sources: ['wkbw'] },
  { name: 'Spectrum News', type: 'Broadcast', sources: ['spectrum-jan2021'] },
  { name: 'WENY', type: 'Broadcast', sources: ['weny'] },
  { name: 'Scripps stations', type: 'Syndicated', sources: ['wkbw'] },
  { name: "CBC Radio's Information Morning (Nova Scotia)", type: 'Radio' },
]

const partners = [
  'WaitList Zero',
  'ONE8FIFTY',
  'Chris Klug Foundation',
]

export default function MediaWall() {
  const { ref, isInView } = useInView(0.1)

  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-copper/5" ref={ref}>
      <div className="content-width">
        <div className="grid grid-cols-1 md:grid-cols-2 gap-16">
          {/* Media Coverage */}
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Press Coverage
            </span>
            <h3 className="font-serif text-xl text-white mb-6">
              National and local outlets.
            </h3>
            <div className="grid grid-cols-2 gap-4">
              {media.map((outlet, i) => (
                <motion.div
                  key={outlet.name}
                  initial={{ opacity: 0, scale: 0.95 }}
                  animate={isInView ? { opacity: 1, scale: 1 } : {}}
                  transition={{ duration: 0.4, delay: 0.2 + i * 0.08 }}
                  className="glass rounded-lg p-4 text-center hover:bg-white/10 transition-colors"
                >
                  <span className="text-white text-sm font-medium">
                    {outlet.name}
                    {outlet.sources && <Cite sources={S} id={outlet.sources} />}
                  </span>
                  <span className="block text-muted text-xs mt-1 font-mono">{outlet.type}</span>
                </motion.div>
              ))}
            </div>
          </motion.div>

          {/* Partners */}
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.2 }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Coalition Partners
            </span>
            <h3 className="font-serif text-xl text-white mb-6">
              Organizations we worked with.
            </h3>
            <div className="space-y-4">
              {partners.map((partner, i) => (
                <motion.div
                  key={partner}
                  initial={{ opacity: 0, x: 20 }}
                  animate={isInView ? { opacity: 1, x: 0 } : {}}
                  transition={{ duration: 0.4, delay: 0.3 + i * 0.1 }}
                  className="glass rounded-lg p-5"
                >
                  <span className="text-white font-serif text-lg">{partner}</span>
                </motion.div>
              ))}
            </div>
          </motion.div>
        </div>
      </div>
    </section>
  )
}
