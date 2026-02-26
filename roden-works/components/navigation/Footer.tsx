import Link from 'next/link'
import { SITE_CONFIG, NAV_ITEMS } from '@/lib/constants'

export default function Footer() {
  const currentYear = new Date().getFullYear()

  return (
    <footer className="border-t border-white/5 bg-slate-950">
      <div className="content-width py-16 md:py-24">
        <div className="grid grid-cols-1 md:grid-cols-4 gap-12 md:gap-8">
          {/* Brand */}
          <div className="md:col-span-2">
            <Link href="/" className="font-serif text-2xl font-bold text-white">
              RODEN
            </Link>
            <p className="mt-4 text-titanium text-sm max-w-md leading-relaxed">
              Optimizing complex systems to improve human quality of life at the
              intersection of engineering, sustainability, and public advocacy.
            </p>
            <div className="mt-6 flex gap-4">
              <a
                href={SITE_CONFIG.linkedin}
                target="_blank"
                rel="noopener noreferrer"
                className="text-titanium hover:text-white transition-colors text-sm"
              >
                LinkedIn
              </a>
              <a
                href={SITE_CONFIG.instagram}
                target="_blank"
                rel="noopener noreferrer"
                className="text-titanium hover:text-white transition-colors text-sm"
              >
                Instagram
              </a>
              <a
                href={SITE_CONFIG.tiktok}
                target="_blank"
                rel="noopener noreferrer"
                className="text-titanium hover:text-white transition-colors text-sm"
              >
                TikTok
              </a>
            </div>
          </div>

          {/* Navigation */}
          <div>
            <h3 className="font-sans text-sm font-medium text-white mb-4 tracking-wide uppercase">
              Navigate
            </h3>
            <ul className="space-y-3">
              {NAV_ITEMS.map((item) => (
                <li key={item.href}>
                  <Link
                    href={item.href}
                    className="text-titanium hover:text-white transition-colors text-sm"
                  >
                    {item.label}
                  </Link>
                </li>
              ))}
            </ul>
          </div>

          {/* Contact */}
          <div>
            <h3 className="font-sans text-sm font-medium text-white mb-4 tracking-wide uppercase">
              Contact
            </h3>
            <ul className="space-y-3">
              <li>
                <a
                  href={`mailto:${SITE_CONFIG.email}`}
                  className="text-titanium hover:text-white transition-colors text-sm"
                >
                  {SITE_CONFIG.email}
                </a>
              </li>
              <li>
                <span className="text-titanium text-sm">{SITE_CONFIG.phone}</span>
              </li>
              <li>
                <a
                  href={SITE_CONFIG.linkedin}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-titanium hover:text-white transition-colors text-sm"
                >
                  linkedin.com/in/evanroden
                </a>
              </li>
            </ul>
          </div>
        </div>

        {/* Bottom */}
        <div className="mt-16 pt-8 border-t border-white/5 flex flex-col md:flex-row justify-between items-center gap-4">
          <p className="text-titanium/60 text-xs">
            &copy; {currentYear} Evan Joseph Roden. All rights reserved.
          </p>
          <p className="text-titanium/40 text-xs">
            Built with Next.js
          </p>
        </div>
      </div>
    </footer>
  )
}
