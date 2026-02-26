'use client'

import Link from 'next/link'
import { motion, useInView } from 'framer-motion'
import { useRef } from 'react'
import { SITE_CONFIG, NAV_ITEMS } from '@/lib/constants'

export default function Footer() {
  const currentYear = new Date().getFullYear()
  const ref = useRef<HTMLElement>(null)
  const isInView = useInView(ref, { once: true, margin: '-5%' })

  return (
    <footer ref={ref} className="border-t border-white/5 bg-slate-950 relative overflow-hidden">
      {/* Background gradient */}
      <div className="absolute inset-0 pointer-events-none">
        <div
          className="absolute bottom-0 left-1/2 -translate-x-1/2 w-[800px] h-[400px] rounded-full"
          style={{
            background: 'radial-gradient(ellipse, rgba(27,58,45,0.08) 0%, transparent 70%)',
            filter: 'blur(80px)',
          }}
        />
      </div>

      <div className="content-width py-16 md:py-24 relative z-10">
        <div className="grid grid-cols-1 md:grid-cols-4 gap-12 md:gap-8">
          {/* Brand */}
          <motion.div
            className="md:col-span-2"
            initial={{ opacity: 0, y: 20 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
          >
            <Link href="/" className="font-serif text-2xl font-bold text-white hover:text-copper transition-colors">
              RODEN
            </Link>
            <p className="mt-4 text-titanium text-sm max-w-md leading-relaxed">
              Optimizing complex systems to improve human quality of life at the
              intersection of engineering, sustainability, and public advocacy.
            </p>
            <div className="mt-6 flex gap-4">
              {[
                { label: 'LinkedIn', href: SITE_CONFIG.linkedin },
                { label: 'Instagram', href: SITE_CONFIG.instagram },
                { label: 'TikTok', href: SITE_CONFIG.tiktok },
              ].map((link) => (
                <a
                  key={link.label}
                  href={link.href}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-titanium/60 hover:text-copper transition-colors text-sm"
                >
                  {link.label}
                </a>
              ))}
            </div>
          </motion.div>

          {/* Navigation */}
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.1 }}
          >
            <h3 className="font-mono text-xs font-medium text-titanium/40 mb-4 tracking-[0.2em] uppercase">
              Navigate
            </h3>
            <ul className="space-y-3">
              {NAV_ITEMS.map((item) => (
                <li key={item.href}>
                  <Link
                    href={item.href}
                    className="text-titanium/60 hover:text-white transition-colors text-sm"
                  >
                    {item.label}
                  </Link>
                </li>
              ))}
            </ul>
          </motion.div>

          {/* Contact */}
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.2 }}
          >
            <h3 className="font-mono text-xs font-medium text-titanium/40 mb-4 tracking-[0.2em] uppercase">
              Contact
            </h3>
            <ul className="space-y-3">
              <li>
                <a
                  href={`mailto:${SITE_CONFIG.email}`}
                  className="text-titanium/60 hover:text-white transition-colors text-sm"
                >
                  {SITE_CONFIG.email}
                </a>
              </li>
              <li>
                <span className="text-titanium/40 text-sm">{SITE_CONFIG.phone}</span>
              </li>
              <li>
                <a
                  href={SITE_CONFIG.linkedin}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-titanium/60 hover:text-white transition-colors text-sm"
                >
                  linkedin.com/in/evanroden
                </a>
              </li>
            </ul>
          </motion.div>
        </div>

        {/* Bottom */}
        <motion.div
          initial={{ opacity: 0 }}
          animate={isInView ? { opacity: 1 } : {}}
          transition={{ duration: 0.6, delay: 0.3 }}
          className="mt-16 pt-8 border-t border-white/5 flex flex-col md:flex-row justify-between items-center gap-4"
        >
          <p className="text-titanium/40 text-xs">
            &copy; {currentYear} Evan Joseph Roden. All rights reserved.
          </p>
          <p className="text-titanium/20 text-xs font-mono tracking-wider">
            Built with Next.js &middot; Framer Motion &middot; Three.js
          </p>
        </motion.div>
      </div>
    </footer>
  )
}
