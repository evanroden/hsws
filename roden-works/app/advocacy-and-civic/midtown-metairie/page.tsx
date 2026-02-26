'use client'

import { motion } from 'framer-motion'
import Breadcrumbs from '@/components/ui/Breadcrumbs'

export default function MidtownMetairiePage() {
  return (
    <>
      <Breadcrumbs items={[{ label: 'Advocacy', href: '/advocacy-and-civic' }, { label: 'Midtown Metairie' }]} />

      <section className="relative min-h-[70vh] flex items-center bg-gradient-to-br from-slate-950 via-copper/5 to-slate-950">
        <div className="content-width relative z-10 text-center max-w-3xl mx-auto">
          <motion.div initial={{ opacity: 0, y: 30 }} animate={{ opacity: 1, y: 0 }} transition={{ duration: 0.7 }}>
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">Urban Planning</span>
            <h1 className="font-serif text-display text-white">Midtown Metairie</h1>
            <p className="mt-6 text-titanium text-lg leading-relaxed">
              An urban planning proposal for Louisiana&apos;s most populous unincorporated community — reimagining the commercial and civic core of Metairie.
            </p>
          </motion.div>

          <motion.div initial={{ opacity: 0, y: 20 }} animate={{ opacity: 1, y: 0 }} transition={{ duration: 0.6, delay: 0.4 }} className="mt-12 glass rounded-xl p-8 md:p-12">
            <div className="w-16 h-16 rounded-full bg-copper/10 flex items-center justify-center mx-auto mb-6">
              <svg className="w-8 h-8 text-copper" fill="none" stroke="currentColor" viewBox="0 0 24 24" strokeWidth="1.5">
                <path strokeLinecap="round" strokeLinejoin="round" d="M12 6v6h4.5m4.5 0a9 9 0 11-18 0 9 9 0 0118 0z" />
              </svg>
            </div>
            <h2 className="font-serif text-xl text-white mb-4">Full Proposal Coming Soon</h2>
            <p className="text-titanium text-sm leading-relaxed mb-8">
              This proposal addresses the transformation of Metairie&apos;s commercial corridors, building on active planning initiatives including the Fat City Redevelopment ($13M CDBG) and the Clearview City Center conversion ($100M) — envisioning a walkable, mixed-use urban center for Jefferson Parish&apos;s largest community.
            </p>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              {['Mixed-Use Development', 'Transit-Oriented Design', 'Community Green Space'].map((item) => (
                <div key={item} className="bg-white/[0.03] rounded-lg p-4 border border-white/5">
                  <span className="text-titanium text-sm">{item}</span>
                </div>
              ))}
            </div>
          </motion.div>
        </div>
      </section>
    </>
  )
}
