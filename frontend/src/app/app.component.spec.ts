import { TestBed, async } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { PushNotificationsService, SimpleNotificationsModule, NotificationsService } from 'angular2-notifications';

import { AppComponent } from './app.component';

describe('AppComponent', () => {
  beforeEach(async(() => {
    TestBed.configureTestingModule({
        imports: [
            RouterTestingModule,
            SimpleNotificationsModule.forRoot(),
        ],
        declarations: [
            AppComponent
        ],
        providers: [
            PushNotificationsService,
            NotificationsService
        ]
    }).compileComponents();
  }));

  it('should create the app', async(() => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.debugElement.componentInstance;
    expect(app).toBeTruthy();
  }));
});
