import { TestBed, async } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { InfoMessagesService } from './info-messages.service';

import { AppComponent } from './app.component';

describe('AppComponent', () => {
  beforeEach(async(() => {
    TestBed.configureTestingModule({
        imports: [
            RouterTestingModule
        ],
        declarations: [
            AppComponent
        ],
        providers: [
            InfoMessagesService
        ]
    }).compileComponents();
  }));

  it('should create the app', async(() => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.debugElement.componentInstance;
    expect(app).toBeTruthy();
  }));

  it('should show the info messages', async(() => {
      const fixture = TestBed.createComponent(AppComponent);
      const app = fixture.componentInstance;
      const element = fixture.nativeElement;
      app.messagesService.infoMessage = 'info msg';
      fixture.detectChanges();
      expect(element.querySelector('div.alert.alert-info#message').textContent)
          .toMatch(/^\s*info msg\s*$/);
  }));

  it('should not show the info messages if it is empty', async(() => {
      const fixture = TestBed.createComponent(AppComponent);
      const app = fixture.componentInstance;
      const element = fixture.nativeElement;
      app.messagesService.infoMessage = '';
      fixture.detectChanges();
      expect(element.querySelector('div.alert.alert-info#message'))
          .toBeNull();
  }));
});
